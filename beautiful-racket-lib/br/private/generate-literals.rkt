#lang racket/base
(require "syntax-flatten.rkt" racket/list racket/match)
(provide ellipses-follow-wildcards-or-subpatterns? generate-literals generate-bound-and-unbound-literals)

(define (literal-identifier? pat-datum)
  (and (symbol? pat-datum)
       (not (memq pat-datum '(... _))) ; isn't a reserved identifier
       (let ([pat-str (symbol->string pat-datum)])
         (or (not (regexp-match #rx"[A-Z]" pat-str)) ; either doesn't contain at least one uppercase letter ...
             (not (equal? (string-upcase pat-str) pat-str)))))) ;...  or doesn't contain all uppercase letters

(define (wildcard? pat-datum)
  (and (symbol? pat-datum)
       (not (literal-identifier? pat-datum))
       (not (memq pat-datum '(... _)))))

;; generate literals for any symbols that are not ... or _ and not IN_CAPS
(define (generate-literals pats)
  (for*/list ([pat-arg (in-list (syntax-flatten pats))]
              [pat-datum (in-value (syntax->datum pat-arg))]
              #:when (literal-identifier? pat-datum))
    pat-arg))


(define (generate-bound-and-unbound-literals pats #:treat-as-bound [bound-id #f])
  (when (and bound-id (not (identifier? bound-id)))
    (raise-argument-error 'generate-bound-and-unbound-literals "identifier" bound-id))
  (define literals (for/list ([literal (in-list (generate-literals pats))]
                              ; the bound-id should not appear in any literal list
                              #:unless (and bound-id (bound-identifier=? literal bound-id)))
                     literal))
  (define-values (bound-literals unbound-literals)
    (partition (λ (i) (or (identifier-binding i)
                          (and bound-id (bound-identifier=? i bound-id)))) literals))
  ;; return as list of two lists so it's easy to match them in syntax pattern
  ;; `syntax-parse` crabs if there are any duplicate ids, so remove them
  (map (λ (ids) (remove-duplicates ids bound-identifier=?)) (list bound-literals unbound-literals)))


(define (ellipses-follow-wildcards-or-subpatterns? pat)
  (let loop ([datum (syntax->datum pat)])
    (match datum
      [(? null?) #t]
      [(cons '... _) #f]
      [(cons _ '...) #f]
      [(list head '... tail ...) (and (or (wildcard? head) (pair? head))
                                      (loop head)
                                      (loop tail))]
      [(list head tail ...) (and (loop head) (loop tail))]
      [(cons x y) (loop (list x y))]
      [else #t])))


(module+ test
  (require rackunit)
  (check-equal? (map syntax->datum (generate-literals #'(foo 42 BAR _ (... ...) bar <=> 3Bar 3bar))) '(foo bar <=> 3Bar 3bar))

  (test-case "wildcard-identifier?"
             (check-true (wildcard? 'FOO))
             (check-true (wildcard? 'TOPPING))

             (check-false (wildcard? 'piZZa))
             (check-false (wildcard? 'please)))

  (test-case "all-...-follow-wildcards"
             (check-true (ellipses-follow-wildcards-or-subpatterns? #'()))
             (check-true (ellipses-follow-wildcards-or-subpatterns? #'foo))
             (check-true (ellipses-follow-wildcards-or-subpatterns? #'(foo . bar)))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(a b))))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(a b C ...))))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '((a b) ...))))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '((C D) ...))))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '((C ...) ...))))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(((C ...) ...) ...))))
             (check-true (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(((C ...) D ...) ...))))

             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(a ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(A ... b ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '((a ...) ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(((a ...) ...) ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(((B ...) a ...) ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '((...) B ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(((((...))))))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(A . ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(... A))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(... . A))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '(... . ...))))
             (check-false (ellipses-follow-wildcards-or-subpatterns? (datum->syntax #f '((... . A) ...))))))

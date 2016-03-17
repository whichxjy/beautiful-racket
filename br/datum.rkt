#lang racket/base
(require (for-syntax racket/base br/syntax) br/define)
(provide (all-defined-out))

;; read "foo bar" the same way as "(foo bar)" 
;; other "bar" is dropped, which is too astonishing
(define (string->datum str)
  (let ([result (read (open-input-string (format "(~a)" str)))])
    (if (= (length result) 1)
        (car result)
        result)))

(define-syntax format-datum
  (λ(stx)
    (syntax-case stx (quote datum)
      [(_ (quote <datum-template>) <arg> ...)
      #'(format-datum (datum <datum-template>) <arg> ...)]
      [(_ (datum datum-template) <arg> ...)
       (syntax-let ([#'format-string (format "~a" (syntax->datum #'datum-template))])
         #'(string->datum (apply format format-string (map (λ(arg) (if (syntax? arg)
                                                                       (syntax->datum arg)
                                                                       arg)) (list <arg> ...)))))])))


(module+ test
  (require rackunit)
  (check-equal? (string->datum "foo") 'foo)
  (check-equal? (string->datum "(foo bar)") '(foo bar))
  (check-equal? (string->datum "foo bar") '(foo bar))
  (check-equal? (string->datum "42") 42)
  (check-equal? (format-datum '(~a-bar-~a) "foo" "zam") '(foo-bar-zam))
  (check-equal? (format-datum (datum (~a-bar-~a)) "foo" "zam") '(foo-bar-zam))
  (check-equal? (format-datum '~a "foo") 'foo)
  (check-equal? (format-datum (datum ~a) "foo") 'foo))

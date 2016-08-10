#lang racket/base
(require (for-syntax racket/base br/syntax) br/define)
(provide (all-defined-out))

;; read "foo bar" the same way as "(foo bar)" 
;; otherwise "bar" is dropped, which is too astonishing
(define (string->datum str)
  (if (positive? (string-length str))
      (let ([result (read (open-input-string (format "(~a)" str)))])
        (if (= (length result) 1)
            (car result)
            result))
      (void)))

#;(define-syntax format-datum
    (λ(stx)
      (syntax-case stx (quote datum)
        [(_ (quote <datum-template>) <arg> ...)
         #'(format-datum (datum <datum-template>) <arg> ...)]
        [(_ (datum datum-template) <arg> ...)
         (syntax-let ([#'format-string (format "~a" (syntax->datum #'datum-template))])
                     #'(string->datum (apply format format-string (map (λ(arg) (if (syntax? arg)
                                                                                   (syntax->datum arg)
                                                                                   arg)) (list <arg> ...)))))])))

(define (datum? x) (or (list? x) (symbol? x)))

(define (format-datum datum-template . args)
  (string->datum (apply format (format "~a" datum-template) (map (λ(arg) (if (syntax? arg)
                                                                             (syntax->datum arg)
                                                                             arg)) args))))

;; todo: rephrase errors from `format` or `map` in terms of `format-datums`
(define (format-datums datum-template . argss)
  (apply map (λ args (apply format-datum datum-template args)) argss))

(module+ test
  (require rackunit syntax/datum)
  (check-equal? (string->datum "foo") 'foo)
  (check-equal? (string->datum "(foo bar)") '(foo bar))
  (check-equal? (string->datum "foo bar") '(foo bar))
  (check-equal? (string->datum "42") 42)
  (check-equal? (format-datum '(~a-bar-~a) "foo" "zam") '(foo-bar-zam))
  (check-equal? (format-datum '(~a-bar-~a) #'foo #'zam) '(foo-bar-zam))
  (check-equal? (format-datum (datum (~a-bar-~a)) "foo" "zam") '(foo-bar-zam))
  (check-equal? (format-datum '~a "foo") 'foo)
  (check-equal? (format-datum (datum ~a) "foo") 'foo)
  (check-equal? (format-datums '(put ~a) '("foo" "zam")) '((put foo) (put zam))))

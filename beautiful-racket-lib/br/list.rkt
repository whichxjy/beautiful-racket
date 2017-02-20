#lang racket/base
(require br/define (for-syntax racket/base))
(provide (all-defined-out))
         
(define-macro (values->list EXPR)
  #'(call-with-values (λ () EXPR) list))

(define-macro (push! ID VAL)
  #'(set! ID (cons VAL ID)))

(define-macro (pop! ID)
  #'(let ([x (car ID)])
      (set! ID (cdr ID))
      x))

(module+ test
  (require rackunit)
  (check-equal? '(1 2 3) (values->list (values 1 2 3)))
  (check-equal? (let ([xs '(2 3)])
                  (push! xs 1)
                  xs) '(1 2 3))
  (check-equal? (let ([xs '(1 2 3)])
                  (define x (pop! xs))
                  (cons x xs)) '(1 2 3)))
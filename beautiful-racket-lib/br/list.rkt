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
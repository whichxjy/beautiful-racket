#lang br
(provide b-expr b-sum b-product b-neg b-expt b-def b-func)

(define (b-expr expr)
  (if (integer? expr) (inexact->exact expr) expr))

(define-macro-cases b-sum
  [(_ VAL) #'VAL]
  [(_ LEFT "+" RIGHT) #'(+ LEFT RIGHT)]
  [(_ LEFT "-" RIGHT) #'(- LEFT RIGHT)])

(define-macro-cases b-product
  [(_ VAL) #'VAL]
  [(_ LEFT "*" RIGHT) #'(* LEFT RIGHT)]
  [(_ LEFT "/" RIGHT) #'(/ LEFT RIGHT 1.0)]
  [(_ LEFT "mod" RIGHT) #'(modulo LEFT RIGHT)])

(define-macro-cases b-neg
  [(_ VAL) #'VAL]
  [(_ "-" VAL) #'(- VAL)])

(define-macro-cases b-expt
  [(_ VAL) #'VAL]
  [(_ LEFT "^" RIGHT) #'(expt LEFT RIGHT)])

(define-macro (b-def ID VAR-ID ... EXPR)
  #'(set! ID (λ (VAR-ID ...) EXPR)))

(define-macro (b-func ID EXPR ...)
  #'(let ([result (ID EXPR ...)])
      (if (boolean? result)
          (if result 1 0)
          result)))

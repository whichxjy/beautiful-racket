#lang br
(require "line.rkt")
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

(define-macro (b-def FUNC-ID VAR-ID ... EXPR)
  (syntax-local-lift-expression
   #'(set! FUNC-ID (λ (VAR-ID ...) EXPR))))

(define-macro (b-func FUNC-ID ARG ...)
  #'(if (procedure? FUNC-ID)
        (let ([result (FUNC-ID ARG ...)])
          (if (boolean? result)
              (if result 1 0)
              result))
        (raise-line-error
         (format "undefined function: ~a" 'FUNC-ID))))

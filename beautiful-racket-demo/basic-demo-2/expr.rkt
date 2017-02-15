#lang br
(provide (all-defined-out))

;; b-sum : b-product (("+" | "-")  b-product)*
(define-macro-cases b-sum
  [(_ PROD) #'PROD]
  [(_ LEFT-PROD "+" RIGHT-PROD) #'(+ LEFT-PROD RIGHT-PROD)]
  [(_ LEFT-PROD "-" RIGHT-PROD) #'(- LEFT-PROD RIGHT-PROD)])

;; b-product : [b-product ("*"|"/"|"%"|"^")] b-value
(define-macro-cases b-product
  [(_ VAL) #'VAL]
  [(_ LEFT "*" RIGHT) #'(* LEFT RIGHT)]
  [(_ LEFT "/" RIGHT) #'(/ LEFT RIGHT 1.0)]
  [(_ LEFT "mod" RIGHT) #'(modulo LEFT RIGHT)])

;; b-expt : [b-expt "^"] b-value
(define-macro-cases b-expt
  [(_ VAL) #'VAL]
  [(_ LEFT "^" RIGHT) #'(expt LEFT RIGHT)])

(define (b-expr expr)
  (if (integer? expr) (inexact->exact expr) expr))

(define (b-negative num) (- num))

(define (b-not expr) (if (zero? expr) 1 0))

(define-macro (b-def ID VAR EXPR)
  #'(set! ID (λ (VAR) EXPR)))

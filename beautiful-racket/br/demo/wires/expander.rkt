#lang br/quicklang
(define-macro (wires-module-begin WIRE-DATUM ...)
  #'(#%module-begin
     WIRE-DATUM ...))
(provide (rename-out [wires-module-begin #%module-begin]))

(define-macro-cases wire
  [(wire VAL -> ID) #'(define (ID)
                        (val VAL))]
  [(wire OP VAL -> ID) #'(define (ID)
                           (OP (val VAL)))]
  [(wire VAL1 OP VAL2 -> ID) #'(define (ID)
                                 (OP (val VAL1) (val VAL2)))]
  [else #'(void)])
(provide wire)

(define-macro (print-wire ID)
  #'(println (format "~a: ~a" 'ID (ID))))

(define wire-cache (make-hash))
(define (val num-or-func)
  (if (number? num-or-func)
      num-or-func
      (hash-ref! wire-cache num-or-func num-or-func)))

(define (16bitize x)
  (define 16bit-max (expt 2 16))
  (define r (modulo x 16bit-max))
  (if (negative? r)
      (16bitize (+ 16bit-max r))
      r))
 
(define-macro (define-16bit ID PROC)
  #'(define ID (compose1 16bitize PROC)))

(define-16bit AND bitwise-and)
(define-16bit OR bitwise-ior)
(define-16bit LSHIFT arithmetic-shift)
(define-16bit RSHIFT (λ(x y) (arithmetic-shift x (- y))))
(define-16bit NOT bitwise-not)
(provide AND OR LSHIFT RSHIFT NOT)
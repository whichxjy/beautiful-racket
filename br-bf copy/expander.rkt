#lang br
(provide #%module-begin #%top-interaction bf-program op loop)

(define #'(bf-program <op-or-loop> ...)
  #'(begin <op-or-loop> ...))

(define-cases #'op
  [#'(_ ">") #'(move-pointer 1)]
  [#'(_ "<") #'(move-pointer -1)]
  [#'(_ "+") #'(set-pointer-byte! (add1 (get-pointer-byte)))]
  [#'(_ "-") #'(set-pointer-byte! (sub1 (get-pointer-byte)))]
  [#'(_ ".") #'(write-byte (get-pointer-byte))]
  [#'(_ ",") #'(set-pointer-byte! (read-byte))])

(define #'(loop "[" <op-or-loop> ... "]")
  #'(until (zero? (get-pointer-byte))
           <op-or-loop> ...))

(define bf-vector (make-vector 1000 0))
(define bf-pointer 0)
(define (get-pointer-byte) (vector-ref bf-vector bf-pointer))
(define (set-pointer-byte! val) (vector-set! bf-vector bf-pointer val))
(define (move-pointer how-far) (set! bf-pointer (+ bf-pointer how-far)))
#lang br/quicklang
 
(provide #%module-begin)

(define-macro (bf-program OP-OR-LOOP ...)
  #'(begin OP-OR-LOOP ...))
(provide bf-program)

(define-macro-cases op
  [(op ">") #'(move-pointer 1)]
  [(op "<") #'(move-pointer -1)]
  [(op "+") #'(set-current-byte! (add1 (get-current-byte)))]
  [(op "-") #'(set-current-byte! (sub1 (get-current-byte)))]
  [(op ".") #'(write-byte (get-current-byte))]
  [(op ",") #'(set-current-byte! (read-byte))])
(provide op)

(define bf-vector (make-vector 30000 0))
(define bf-pointer 0)

(define (move-pointer how-far) (set! bf-pointer (+ bf-pointer how-far)))

(define (get-current-byte) (vector-ref bf-vector bf-pointer))
(define (set-current-byte! val)  (vector-set! bf-vector bf-pointer val))

(define-macro (loop LOOP-ARG ...)
  #'(until (zero? (get-current-byte))
           LOOP-ARG ...))
(provide loop)

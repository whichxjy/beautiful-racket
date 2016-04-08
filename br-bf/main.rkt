#lang br
(provide #%module-begin #%top-interaction
         bf-program op loop)

(module reader syntax/module-reader
  #:language 'br-bf
  #:read bf-read
  #:read-syntax bf-read-syntax
  ;; need this because we keep state,
  ;; therefore expansion is "all or nothing"
  #:whole-body-readers? #t 
  
  (require "tokenizer.rkt" "parser.rkt")
  (define (bf-read in)
    (syntax->datum (bf-read-syntax #f in)))
  
  (define (bf-read-syntax src ip)
    (define result (list (parse src (tokenize ip))))
    ;; prints out corresponding s-exp source
    (for-each println (map syntax->datum result))
    result))

(define #'(bf-program <op-or-loop> ...)
  #'(begin <op-or-loop> ...))

(define-cases #'op
    [#'(_ ">") #'(move-pointer 1)]
    [#'(_ "<") #'(move-pointer -1)]
    [#'(_ "+") #'(set-pointer-byte! (add1 (get-pointer-byte)))]
    [#'(_ "-") #'(set-pointer-byte! (sub1 (get-pointer-byte)))]
    [#'(_ ".") #'(write-byte (get-pointer-byte))]
    [#'(_ ",") #'(set-pointer-byte! (read-byte))])

(define-cases f
  [(_ arg) (add1 arg)]
  [(_ arg1 arg2) (+ arg1 arg2)])

(define #'(loop "[" <op-or-loop> ... "]")
  #'(until (zero? (get-pointer-byte))
           <op-or-loop> ...))

(define bf-vector (make-vector 1000 0))
(define bf-pointer 0)
(define (get-pointer-byte) (vector-ref bf-vector bf-pointer))
(define (set-pointer-byte! val) (vector-set! bf-vector bf-pointer val))
(define (move-pointer how-far) (set! bf-pointer (+ bf-pointer how-far)))

(define (dump)
  (displayln "")
  (displayln bf-pointer)
  (displayln bf-vector))




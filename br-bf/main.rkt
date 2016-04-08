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


(define #'(bf-program arg ...)
  #'(begin arg ...))

(define #'(op arg)
  (case (syntax->datum #'arg)
    [(">") #'(move-pointer 1)]
    [("<") #'(move-pointer -1)]
    [("+") #'(set-pointer-byte! (add1 (pointer-byte)))]
    [("-") #'(set-pointer-byte! (sub1 (pointer-byte)))]
    [(".") #'(write-byte (pointer-byte))]
    [(",") #'(set-pointer-byte! (read-byte (current-input-port)))]
    [else #'arg]))

(define #'(loop lb arg ... rb)
  #'(let loop ()
      (unless (zero? (pointer-byte))
        arg ...
        (loop))))

(define bf-vector (make-vector 10 0))
(define bf-pointer 0)
(define (pointer-byte) (vector-ref bf-vector bf-pointer))
(define (set-pointer-byte! val) (vector-set! bf-vector bf-pointer val))

(define (move-pointer how-far)
  (set! bf-pointer (+ bf-pointer how-far)))

(define (dump)
  (displayln "")
  (displayln bf-pointer)
  (displayln bf-vector))




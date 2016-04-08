#lang br
(provide (all-from-out br)
         (all-defined-out))

(define bf-vector (make-vector 1000 0))
(define bf-pointer 0)
(define (byte-at-pointer) (vector-ref bf-vector bf-pointer))
(define (change-byte-at-pointer val) (vector-set! bf-vector bf-pointer val))

(define (change-pointer how-far)
  (set! bf-pointer (+ bf-pointer how-far)))

(define (change-pointer-val how-much)
  (change-byte-at-pointer (+ (byte-at-pointer) how-much)))

(define #'(bf-program arg ...)
  #'(begin arg ...))

(define #'(expr arg)
  (case (syntax->datum #'arg)
    [(">") #'(change-pointer 1)]
    [("<") #'(change-pointer -1)]
    [("+") #'(change-pointer-val 1)]
    [("-") #'(change-pointer-val -1)]
    [(".") #'(write-byte (byte-at-pointer))]
    [(",") #'(change-byte-at-pointer (read-byte (current-input-port)))]
    [else #'arg]))

(define #'(loop lb arg ... rb)
  #'(let loop ()
      (unless (zero? (vector-ref bf-vector bf-pointer))
        arg ...
        (loop))))

(module reader syntax/module-reader
  #:language 'br-bf
  #:read bf-read
  #:read-syntax bf-read-syntax
  #:whole-body-readers? #t
  
  (require "tokenizer.rkt" "parser.rkt")
  (define (bf-read in)
    (syntax->datum (bf-read-syntax #f in)))
  
  (define (bf-read-syntax src ip)
    (list (parse src (tokenize ip)))))

#lang br


(module reader syntax/module-reader
  #:language 'br-bf
  #:read bf-read
  #:read-syntax bf-read-syntax
  
  (require "tokenizer.rkt" "parser.rkt")
  (define (bf-read in)
    (syntax->datum (bf-read-syntax #f in)))
  
  (define (bf-read-syntax src ip)
    (list (parse src (tokenize ip)))))

#lang br/quicklang

(module+ reader
  (provide read-syntax))

(define (tokenize ip)
  (for/list ([tok (in-port read-char ip)])
            tok))

(define (parse-char c)
  (define int (modulo (char->integer c) 128))
  (for/list ([bit (in-range 7)])
            (if (bitwise-bit-set? int bit)
                'taco
                null)))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (map parse-char toks))
  (strip-context
   (with-syntax ([(PARSED-CHAR ...) parse-tree])
     #'(module tacofied racket
         (for-each displayln '(PARSED-CHAR ...))))))
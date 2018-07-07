#lang br/quicklang
(require brag/support "grammar.rkt")
(provide (all-from-out br/quicklang) (all-defined-out))

(module+ reader
  (provide read-syntax))

(define lex
  (lexer
   ["#$" lexeme]
   ["%" lexeme]
   [any-char (lex input-port)]))

(define (taco-program . pieces) pieces)

(define (taco-leaf . pieces)
  (integer->char
   (for/sum ([bit (in-list pieces)]
             [pow (in-naturals)])
            (* bit (expt 2 pow)))))

(define (taco) 1)

(define (not-a-taco) 0)

(define (read-syntax src ip)
  (define parse-tree (parse (λ () (lex ip))))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module vic taco-victory-demo
         (display (apply string PT))))))
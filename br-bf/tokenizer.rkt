#lang racket/base
(require parser-tools/lex ragg/support racket/function)
(provide tokenize lex)

(define lex
  (lexer-src-pos
   [(char-set "><-.,+[]") lexeme]
   [whitespace (token '_ lexeme #:skip? #t)]
   [(eof) (void)]))

(define (tokenize ip)
  (port-count-lines! ip)  
  (define next-token-thunk (thunk (lex ip)))
  next-token-thunk)

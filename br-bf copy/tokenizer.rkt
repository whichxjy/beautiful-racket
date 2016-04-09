#lang racket/base
(require parser-tools/lex ragg/support)
(provide tokenize)

(define (tokenize ip)
  (port-count-lines! ip)
  
  (define lex
    (lexer
     [(char-set "><-.,+[]") lexeme]
     [whitespace (token 'white #:skip? #t)]
     [(eof) (void)]))
  
  (define next-token-func (λ _ (lex ip)))
  next-token-func)

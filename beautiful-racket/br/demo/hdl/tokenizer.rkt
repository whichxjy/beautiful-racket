#lang br
(require parser-tools/lex parser-tools/lex-sre
         ragg/support
         racket/string)

(provide tokenize)
(define (tokenize input-port)
  (define (next-token)
    (define get-token
      (lexer
       [(eof) eof]
       [(union #\tab #\space #\newline) (get-token input-port)]
       [(union "CHIP" "IN" "OUT" "PARTS:") lexeme]
       [(char-set "{}(),;=") lexeme]
       [(repetition 1 +inf.0 (union alphabetic numeric)) (token 'ID (string->symbol lexeme))]))
    (get-token input-port))  
  next-token)

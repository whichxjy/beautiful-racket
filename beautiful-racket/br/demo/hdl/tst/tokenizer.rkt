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
       [(seq "/*" (complement (seq any-string "*/" any-string)) "*/")
        (token 'COMMENT lexeme #:skip? #t)]
       [(union #\tab #\space #\newline) (get-token input-port)]
       [(union "load" "output-list" "set" "eval" "output" "out" (char-set ",;")) lexeme]
       [(repetition 1 +inf.0 numeric) (token 'VAL (string->number lexeme))]
       [(repetition 1 +inf.0 (union alphabetic numeric (char-set "-."))) (token 'ID (string->symbol lexeme))]))
    (get-token input-port))  
  next-token)

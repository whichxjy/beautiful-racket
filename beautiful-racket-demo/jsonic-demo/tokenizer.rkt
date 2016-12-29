#lang br/quicklang
(require brag/support)

(define (tokenize port)
  (define (next-token)
    (define our-lexer
      (lexer
       [(eof) eof]
       [(from/to "//" "\n") (next-token)]
       [(from/to "@$" "$@")
        (token 'SEXP-TOK (trim-ends "@$" lexeme "$@"))]
       [any-char (token 'CHAR-TOK lexeme)]))
    (our-lexer port))  
  next-token)
(provide tokenize)
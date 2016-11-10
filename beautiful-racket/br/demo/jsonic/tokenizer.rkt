#lang br/quicklang
(require brag/support)
(define (tokenize port)
  (define (next-token)
    (define our-lexer
      (lexer
       [(eof) eof]
       [(between "//" "\n") (next-token)]
       [(between "@$" "$@")
        (token 'SEXP-TOK (trim-delimiters "@$" lexeme "$@"))]
       [any-char (token 'CHAR-TOK lexeme)]))
    (our-lexer port))  
  next-token)
(provide tokenize)
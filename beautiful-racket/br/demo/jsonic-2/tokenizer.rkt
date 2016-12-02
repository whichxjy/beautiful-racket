#lang br/quicklang
(require brag/support)

(define (token? x)
  (or (eof-object? x) (string? x) (token-struct? x)))

(define/contract (tokenize port)
  (input-port? . -> . (-> token?))
  (define/contract (next-token)
    (-> token?)
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
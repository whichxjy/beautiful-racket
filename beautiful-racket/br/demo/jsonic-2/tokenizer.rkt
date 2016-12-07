#lang br/quicklang
(require brag/support)

(module+ test
  (require rackunit))

(define (token? x)
  (or (eof-object? x) (string? x) (token-struct? x)))

(module+ test
  (check-true (token? eof))
  (check-true (token? "a string"))
  (check-true (token? (token 'A-TOKEN-STRUCT "hi")))
  (check-false (token? 42)))

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

(module+ test
  (check-equal? (apply-tokenizer tokenize "// comment\n") empty)
  (check-equal?
   (apply-tokenizer tokenize "@$ (+ 6 7) $@")
   (list (token-struct 'SEXP-TOK " (+ 6 7) " #f #f #f #f #f)))
  (check-equal?
   (apply-tokenizer tokenize "hi")
   (list (token-struct 'CHAR-TOK "h" #f #f #f #f #f)
         (token-struct 'CHAR-TOK "i" #f #f #f #f #f))))
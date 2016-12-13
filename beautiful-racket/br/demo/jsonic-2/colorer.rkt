#lang br
(require brag/support
         syntax-color/racket-lexer)

(define in-racket-expr? #f)

(define (color-jsonic port)
  (define jsonic-lexer
    (lexer
     [(eof) (values lexeme 'eof #f #f #f)]
     ["@$" (begin
             (set! in-racket-expr? #t)
             (values lexeme 'parenthesis '|(|
                     (pos lexeme-start) (pos lexeme-end)))]
     ["$@" (begin
             (set! in-racket-expr? #f)
             (values lexeme 'parenthesis '|)|
                     (pos lexeme-start) (pos lexeme-end)))]
     [(from/to "//" "\n")
      (values lexeme 'comment #f (pos lexeme-start) (pos lexeme-end))]
     [any-char
      (values lexeme 'string #f (pos lexeme-start) (pos lexeme-end))]))
  (if (and in-racket-expr? (not (equal? (peek-string 2 0 port) "$@")))
      (racket-lexer port)
      (jsonic-lexer port)))
(provide color-jsonic)
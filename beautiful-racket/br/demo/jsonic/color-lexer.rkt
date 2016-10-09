#lang br
(require parser-tools/lex
         syntax-color/racket-lexer
         parser-tools/lex-sre)
  
(provide color-lexer)

(define in-racket-expr? #f)

(define (at-racket-boundary? input-port)
  (equal? (peek-string 2 0 input-port) "$@"))

(module+ test
  (require rackunit)
  (check-true (at-racket-boundary? (open-input-string "$@foo")))
  (check-false (at-racket-boundary? (open-input-string "foo$@"))))

(define (color-lexer input-port)
  (define jsonic-lexer
    (lexer
     [(eof) (values lexeme 'eof #f #f #f)]
     ["@$" (begin
             (set! in-racket-expr? #t)
             (values lexeme 'parenthesis '|(| (position-offset start-pos) (position-offset end-pos)))]
     ["$@" (begin
             (set! in-racket-expr? #f)
             (values lexeme 'parenthesis '|)| (position-offset start-pos) (position-offset end-pos)))]
     [(seq "//" (* (char-complement #\newline)))
      (values lexeme 'comment #f (position-offset start-pos) (position-offset end-pos))]
     [any-char
      (values lexeme 'string #f (position-offset start-pos) (position-offset end-pos))]))
  (if (and in-racket-expr? (not (at-racket-boundary? input-port)))
      (racket-lexer input-port)
      (jsonic-lexer input-port)))
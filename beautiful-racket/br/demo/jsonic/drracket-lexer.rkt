#lang br

(require parser-tools/lex
         syntax-color/racket-lexer
         (prefix-in : parser-tools/lex-sre))
  
(provide drracket-lexer)

(define drracket-lexer
  (let ([in-racket-expr? #f])
    (lexer
     [(eof) (values lexeme 'eof #f #f #f)]
     ["@$" (begin
             (set! in-racket-expr? #t)
             (values lexeme 'parenthesis '|(| (position-offset start-pos) (position-offset end-pos)))]
     ["$@" (begin
             (set! in-racket-expr? #f)
             (values lexeme 'parenthesis '|)| (position-offset start-pos) (position-offset end-pos)))]
     [(:seq "//" (:* (char-complement #\newline)))
      (values lexeme 'comment #f (position-offset start-pos) (position-offset end-pos))]
     [any-char
      (if in-racket-expr?
          (racket-lexer (transplant-input-port (open-input-string lexeme) #f (position-offset start-pos)))
          (values lexeme 'string #f (position-offset start-pos) (position-offset end-pos)))])))
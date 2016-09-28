#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
  
(provide default-lexer)
                     
  
(define default-lexer
  (lexer
   [(eof) (values lexeme 'eof #f #f #f)]
   [(:seq "//" (:* (char-complement "\n")))
    (values lexeme 'comment #f (position-offset start-pos) (position-offset end-pos))]
   [any-char (values lexeme 'no-color #f (position-offset start-pos) (position-offset end-pos))]))
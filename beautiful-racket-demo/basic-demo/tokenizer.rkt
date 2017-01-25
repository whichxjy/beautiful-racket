#lang br
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         brag/support)
(provide (all-defined-out))

(define basic-lexer
  (lexer-src-pos
   [(eof) eof]
   [whitespace (token lexeme #:skip? #t)]
   [(from/to "rem" "\n") (token 'REM (string-downcase lexeme))]
   [(:or "print" "goto" "end") (token (string-downcase lexeme)
                                      (string-downcase lexeme))]
   [(:+ numeric) (token 'NUMBER (string->number lexeme))]
   [(from/to "\"" "\"") (token 'STRING (trim-ends  "\"" lexeme "\""))]))

(define (tokenize ip)
  (port-count-lines! ip)
  (λ () (basic-lexer ip)))
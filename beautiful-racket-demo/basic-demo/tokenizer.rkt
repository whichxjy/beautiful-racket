#lang br
(require brag/support)
(provide (all-defined-out))

(define basic-lexer
  (lexer-srcloc
   [(eof) eof]
   [whitespace (token lexeme #:skip? #t)]
   [(from/to "rem" "\n") (token 'REM lexeme)]
   [(:or "print" "goto" "end" "+" ":") lexeme]
   [(:+ numeric) (token 'INTEGER (string->number lexeme))]
   [(:or (:seq (:+ numeric) ".")
         (:seq (:* numeric) "." (:+ numeric)))
    (token 'DECIMAL (string->number lexeme))]
   [(from/to "\"" "\"") (token 'STRING (trim-ends  "\"" lexeme "\""))]))

(define (make-tokenizer ip)
  (port-count-lines! ip)
  (define (next-token) (basic-lexer ip))
  next-token)
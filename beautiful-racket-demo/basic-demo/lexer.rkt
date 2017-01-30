#lang br
(require brag/support)

(define basic-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   [whitespace (token lexeme #:skip? #t)]
   [(from/to "rem" #\newline) (token 'REM lexeme)]
   [(:or "print" "goto" "end" "+" ":") lexeme]
   [(:+ numeric) (token 'INTEGER (string->number lexeme))]
   [(:or (:seq (:+ numeric) ".")
         (:seq (:* numeric) "." (:+ numeric)))
    (token 'DECIMAL (string->number lexeme))]
   [(from/to "\"" "\"")
    (token 'STRING (trim-ends "\"" lexeme "\""))]))

(provide basic-lexer)

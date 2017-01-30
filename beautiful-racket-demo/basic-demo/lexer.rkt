#lang br
(require brag/support racket/contract)

(define basic-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   [whitespace (token lexeme #:skip? #t)]
   [(from/to "rem" "\n") (token 'REM lexeme)]
   [(:or "print" "goto" "end" "+" ":") lexeme]
   [(:+ numeric) (token 'INTEGER (string->number lexeme))]
   [(:or (:seq (:+ numeric) ".")
         (:seq (:* numeric) "." (:+ numeric)))
    (token 'DECIMAL (string->number lexeme))]
   [(from/to "\"" "\"")
    (token 'STRING (trim-ends "\"" lexeme "\""))]))

(provide basic-lexer)


(define (apply-lexer lexer str)
(for/list ([t (in-port lexer (open-input-string str))])
  t))

(apply-lexer basic-lexer "10 rem")

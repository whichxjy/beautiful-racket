#lang br
(require brag/support)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define (handle-tok-error tok-ok? tok-name tok-value start-pos end-pos)
  (token 'ERROR tok-value))

(define basic-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [(from/stop-before "rem" "\n") (token 'REM lexeme)]
   [(:or "print" "goto" "end" "+" ":" "gosub" "return" "let" "=" "-" "for" "to" "step" "next"
         "if" "then" "else" "and" "or" "<" ">" "*" "/" "(" ")" "^" "!" "%" "input" ";" "def") (token lexeme lexeme)]
   [(:seq (:+ alphabetic) (:* (:or alphabetic numeric "$"))) (token 'ID (string->symbol lexeme))]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:or (:seq (:? digits) "." digits)
         (:seq digits "."))
    (token 'DECIMAL (string->number lexeme))]
   [(:or (from/to "\"" "\"") (from/to "'" "'"))
    (token 'STRING
           (substring lexeme
                      1 (sub1 (string-length lexeme))))]))

(provide basic-lexer)
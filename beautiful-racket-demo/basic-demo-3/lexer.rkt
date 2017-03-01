#lang br
(require brag/support)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define-lex-abbrev reserved-terms (:or "print" "goto" "end" "+" ":" ";" "let" "=" "input" "-" "*" "/" "^" "mod" "(" ")" "def" "if" "then" "else" "<" ">" "<>" "and" "or" "not" "gosub" "return" "for" "to" "step" "next" "def" "," "import"))

(define-lex-abbrev id-kapu
  (:or whitespace (char-set "()[]{}\",'`;#|\\")))

(define basic-lexer
  (lexer-srcloc
   [(eof) (return-without-srcloc eof)]
   ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [(from/stop-before "rem" "\n") (token 'REM lexeme)]
   [reserved-terms (token lexeme lexeme)]
   [digits (token 'INTEGER (string->number lexeme))]
   [(:or (:seq (:? digits) "." digits)
         (:seq digits "."))
    (token 'DECIMAL (string->number lexeme))]
   [(:seq (:~ (:or "-" id-kapu)) (:* (:~ id-kapu)))
    (token 'ID (string->symbol lexeme))]
   [(:or (from/to "\"" "\"") (from/to "'" "'"))
    (token 'STRING
           (substring lexeme
                      1 (sub1 (string-length lexeme))))]))

(provide basic-lexer)
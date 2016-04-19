#lang br
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         ragg/support
         racket/string)
(provide tokenize)

(define (tokenize input-port)
  (define (next-token)
    (define get-token
      (lexer
       [(repetition 1 +inf.0 "\n") (token 'CR '(CR))]
       [(union "PRINT" "FOR" "TO" "STEP" "IF" "THEN" "GOTO" "REM"
               "INPUT" "LET" "NEXT" "GOSUB" "RETURN"
               "CLEAR" "LIST" "RUN" "END") (string->symbol lexeme)]
       
       ;; this only matches integers
       [(repetition 1 +inf.0 numeric) (token 'INTEGER (string->number lexeme))]
       [(repetition 1 +inf.0 (union "." numeric)) (token 'REAL (string->number lexeme))]
       [(union "," ";" ":" "+" "-" "*" "/"
               "<=" ">=" "<>" "><" "<" ">" "=" "(" ")") lexeme]
       [(:seq (repetition 1 +inf.0 upper-case)) (token 'ID lexeme)]
       [upper-case (token 'UPPERCASE lexeme)]
       [whitespace (token 'WHITESPACE lexeme #:skip? #t)]
       [(:seq "\"" (complement (:: any-string "\"" any-string)) "\"") (token 'STRING (string-trim lexeme "\""))]
       [(eof) eof]))
    (get-token input-port))  
  next-token)

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
       [(:seq "REM" (repetition 1 +inf.0 (char-complement "\n")))
        (token 'REM-COMMENT (format-datum '(comment "~v") lexeme))]
       [(repetition 1 +inf.0 "\n") (token 'CR "cr")]
       [(union "PRINT" "FOR" "TO" "STEP" "IF" "THEN" "GOTO"
               "INPUT" "LET" "NEXT" "GOSUB" "RETURN"
               "CLEAR" "LIST" "RUN" "END") (string->symbol lexeme)]
       
       ;; this only matches integers
       [(repetition 1 +inf.0 numeric) (token 'INTEGER (string->number lexeme))]
       [(repetition 1 +inf.0 (union "." numeric)) (token 'REAL (string->number lexeme))]
       ;; things that get thrown out: pass through as strings,
       ;; because they can be matched literally in macros.
       ;; things that become identifiers: pass through as symbols,
       ;; so they can get bound by the expander.
       [(union "," ":") (token 'SEPARATOR lexeme #:skip? #t)]
       [(union ";" "=" "(" ")") lexeme]
       [(union "+" "-" "*" "/"
               "<=" ">=" "<>" "><" "<" ">" "=" ) (string->symbol lexeme)]
       [(:seq (repetition 1 +inf.0 upper-case)) (token 'ID (string->symbol lexeme))]
       [upper-case (token 'UPPERCASE (string->symbol lexeme))]
       [whitespace (token 'WHITESPACE lexeme #:skip? #t)]
       [(:seq "\"" (complement (:: any-string "\"" any-string)) "\"") (token 'STRING (string-trim lexeme "\""))]
       [(eof) eof]))
    (get-token input-port))  
  next-token)

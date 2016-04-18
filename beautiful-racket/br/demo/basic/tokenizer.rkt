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
       [(:seq "REM" (repetition 1 +inf.0 (char-complement "\n")) (repetition 0 +inf.0 "\n"))
        (token 'COMMENT lexeme #:skip? #t)]
       [(repetition 1 +inf.0 "\n") (token 'CR '(CR))]
       [(union "PRINT" "IF" "THEN" "GOTO"
               "INPUT" "LET" "GOSUB" "RETURN"
               "CLEAR" "LIST" "RUN" "END") (string->symbol lexeme)]
       
       ;; this only matches integers
       [(repetition 1 +inf.0 numeric) (token 'NUMBER (string->number lexeme))]
       [(char-set ",;:+-ε*/<>=()") lexeme]
       [(:seq (repetition 1 +inf.0 upper-case) "(") lexeme]
       [upper-case (token 'UPPERCASE lexeme)]
       [whitespace (token 'WHITESPACE lexeme #:skip? #t)]
       [(:seq "\"" (complement (:: any-string "\"" any-string)) "\"") (token 'STRING (string-trim lexeme "\""))]
       [(eof) eof]))
    (get-token input-port))  
  next-token)

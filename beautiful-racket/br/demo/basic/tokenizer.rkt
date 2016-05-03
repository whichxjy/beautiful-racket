#lang br
(require parser-tools/lex parser-tools/lex-sre
         br/ragg/support
         racket/string)
(provide tokenize)

(define-lex-abbrevs
  (natural (repetition 1 +inf.0 numeric))
  (number (union (seq (? "-") natural)
                 (seq (? "-") (? natural) (seq "." natural))))
  (quoted-string (seq "\"" (repetition 0 +inf.0 (char-complement "\"")) "\"")))

(define (tokenize input-port)
  (define (next-token)
    (define get-token
      (lexer
       [(eof) eof]
       [(union #\tab #\space
               (seq number " REM" (repetition 1 +inf.0 (char-complement #\newline)) #\newline)) (get-token input-port)]
       [(seq #\newline (repetition 0 +inf.0 whitespace)) (token 'CR "cr")]
       [(union "PRINT" "FOR" "TO" "STEP" "IF" "GOTO"
               "INPUT" "LET" "NEXT"  "RETURN"
               "CLEAR" "LIST" "RUN" "END"
               "THEN" "ELSE" "GOSUB" "AND" "OR"
               ";" "=" "(" ")" "+" "-" "*" "/"
               "<=" ">=" "<>" "<" ">" "=" ":") lexeme]
       [(union ",") (get-token input-port)]
       [number (token 'NUMBER (string->number lexeme))]
       [(seq (repetition 1 +inf.0 upper-case) (? "$")) (token 'ID (string->symbol lexeme))]
       [upper-case (token 'UPPERCASE (string->symbol lexeme))]
       [quoted-string (token 'STRING (string-trim lexeme "\""))]))
    (get-token input-port))  
  next-token)

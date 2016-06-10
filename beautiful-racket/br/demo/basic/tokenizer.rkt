#lang br
(require parser-tools/lex parser-tools/lex-sre
         brag/support
         racket/string)
(provide tokenize)

(define-lex-abbrevs
  (natural (repetition 1 +inf.0 numeric))
;; don't lex the leading "-": muddles "-X" and "Y-X"
  (number (union (seq natural)
                 (seq (? natural) (seq "." natural))))
  (quoted-string (seq "\"" (repetition 0 +inf.0 (char-complement "\"")) "\"")))

(define (tokenize input-port)
  (define (next-token)
    (define get-token
      (lexer-src-pos
       [(eof) eof]
       [(seq "/*" (complement (seq any-string "*/" any-string)) "*/") (get-token input-port)]
       [(union #\tab #\space #\newline
               (seq number " REM" (repetition 0 +inf.0 (char-complement #\newline)) #\newline)) (get-token input-port)]
       [(union "PRINT" "print" "FOR" "for" "TO" "to" "STEP" "step" "IF" "if"
               "GOTO" "goto" "INPUT" "input" "LET" "let" "NEXT" "next"
               "RETURN" "return" "CLEAR" "clear" "LIST" "list" "RUN" "run"
               "END" "end" "THEN" "then" "ELSE" "else" "GOSUB" "gosub"
               "AND" "and" "OR" "or" "STOP" "stop" "LET" "let" "DEF" "def"
               ";" "=" "(" ")" "+" "-" "*" "/"
               "<=" ">=" "<>" "<" ">" "=" ":" ",") (string-downcase lexeme)]
       [number (token 'NUMBER (string->number lexeme))]
       [(seq upper-case (repetition 0 +inf.0 (or upper-case numeric)) (? "$")) (token 'ID (string->symbol lexeme))]
       [quoted-string (token 'STRING (string-trim lexeme "\""))]))
    (get-token input-port))  
  next-token)


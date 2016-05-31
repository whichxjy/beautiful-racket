#lang br
(require parser-tools/lex parser-tools/lex-sre
         brag/support
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
      (lexer-src-pos
       [(eof) eof]
       [(union #\tab #\space #\newline
               (seq number " REM" (repetition 1 +inf.0 (char-complement #\newline)) #\newline)) (get-token input-port)]
       [(union "PRINT" "print" "FOR" "for" "TO" "to" "STEP" "step" "IF" "if"
               "GOTO" "goto" "INPUT" "input" "LET" "let" "NEXT" "next"
               "RETURN" "return" "CLEAR" "clear" "LIST" "list" "RUN" "run"
               "END" "end" "THEN" "then" "ELSE" "else" "GOSUB" "gosub"
               "AND" "and" "OR" "or"
               ";" "=" "(" ")" "+" "-" "*" "/"
               "<=" ">=" "<>" "<" ">" "=" ":") (string-downcase lexeme)]
       [(union ",") (get-token input-port)]
       [number (token 'NUMBER (string->number lexeme))]
       [(seq (repetition 1 +inf.0 upper-case) (? "$")) (token 'ID (string->symbol lexeme))]
       [upper-case (token 'UPPERCASE (string->symbol lexeme))]
       [quoted-string (token 'STRING (string-trim lexeme "\""))]))
    (get-token input-port))  
  next-token)

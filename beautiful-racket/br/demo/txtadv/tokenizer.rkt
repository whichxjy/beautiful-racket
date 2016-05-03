#lang br
(require parser-tools/lex parser-tools/lex-sre
         ragg/support
         racket/string)

(provide tokenize)
(define (tokenize input-port)
  (define (next-token)
    (define get-token
      (lexer
       [(eof) eof]
       [(union
         (seq "/*" (complement (seq any-string "*/" any-string)) "*/")
         (seq "//" (repetition 1 +inf.0 (char-complement #\newline)) #\newline))
        (token 'COMMENT lexeme #:skip? #t)]
       [(union #\tab #\space #\newline) (get-token input-port)]
       [(repetition 1 +inf.0 (union upper-case (char-set "="))) lexeme]
       [(seq "\"" (complement (seq any-string "\"" any-string)) "\"") (token 'STRING lexeme)]
       [(seq "---"
             (repetition 1 +inf.0 (union alphabetic numeric punctuation))
             "---") (token 'THING-NAME (string->symbol (string-trim lexeme "-" #:repeat? #t)))]
       [(repetition 1 +inf.0 (union alphabetic numeric (char-set "-!?.#")))
        (token 'ID (read (open-input-string lexeme)))]
       [any-char lexeme]))
    (get-token input-port))  
  next-token)

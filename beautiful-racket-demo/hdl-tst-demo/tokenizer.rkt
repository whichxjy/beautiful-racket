#lang br
(require brag/support
         racket/string)

(provide tokenize)
(define (tokenize input-port)
  (define (next-token)
    (define get-token
      (lexer-src-pos
       [(eof) eof]
        [(union
         (:seq "/*" (complement (:seq any-string "*/" any-string)) "*/")
         (:seq "//" (repetition 1 +inf.0 (char-complement #\newline)) #\newline))
        (token 'COMMENT lexeme #:skip? #t)]
       [(union #\tab #\space #\newline) (return-without-pos (get-token input-port))]
       [(union "load" "output-list" "output-file" "compare-to" "set" "eval" "output" (char-set ",;")) lexeme]
       [(:seq "%" (repetition 1 +inf.0 (union alphabetic numeric (char-set ".")))) (token 'FORMAT-STRING lexeme)]
       [(repetition 1 +inf.0 numeric) (token 'VAL (string->number lexeme))]
       [(repetition 1 +inf.0 (union alphabetic numeric (char-set "-."))) (token 'ID lexeme)]))
    (get-token input-port))  
  next-token)

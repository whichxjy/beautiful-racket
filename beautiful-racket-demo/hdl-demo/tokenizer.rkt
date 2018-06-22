#lang br
(require brag/support
         racket/string)

(provide tokenize)
(define (tokenize input-port)
  (define (next-token)
    (define get-token
      (lexer-src-pos
       [(union
         (:seq "/*" (complement (:seq any-string "*/" any-string)) "*/")
         (:seq "//" (repetition 1 +inf.0 (char-complement #\newline)) #\newline))
        (token 'COMMENT lexeme #:skip? #t)]
       [(union #\tab #\space #\newline) (return-without-pos (get-token input-port))]
       [(union "CHIP" "IN" "OUT" "PARTS:") lexeme]
       [(char-set "[]{}(),;=.") lexeme]
       ["true" (token 'TRUE #t)]
       ["false" (token 'FALSE #f)]
       [(repetition 1 +inf.0 numeric) (token 'NUMBER (string->number lexeme))]
       ; bugaboo: "10" is ambiguous: number or binary number?
       [(repetition 1 +inf.0 (char-set "01")) (token 'BINARY-NUMBER (string->number lexeme 2))]
       [(:seq (repetition 1 1 alphabetic) (repetition 0 +inf.0 (union alphabetic numeric "-"))) (token 'ID (string->symbol lexeme))]))
    (get-token input-port))  
  next-token)

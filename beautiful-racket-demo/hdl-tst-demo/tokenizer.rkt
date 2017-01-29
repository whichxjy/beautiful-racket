#lang br/quicklang
(require brag/support)
(provide make-tokenizer)

(define hdl-test-lexer
  (lexer-srcloc
   [(eof) eof]
   [(:or (from/to "/*" "*/")
         (from/to "//" #\newline)) (token 'COMMENT lexeme #:skip? #t)]
   [whitespace (token lexeme #:skip? #t)]
   [(:or "load" "output-list" "output-file" "compare-to" "set" "eval" "output" "," ";") lexeme]
   [(:seq "%" (:+ alphabetic numeric ".")) (token 'FORMAT-STRING lexeme)]
   [(:+ numeric) (token 'VAL (string->number lexeme))]
   [(:+ alphabetic numeric "-" ".") (token 'ID lexeme)]))

(define (make-tokenizer ip)
  (define (next-token) (hdl-test-lexer ip))  
  next-token)

#lang br
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         brag/support)
(provide (all-defined-out))

(define basic-lexer
  (lexer-src-pos
   [(eof) eof]
   [whitespace (token lexeme #:skip? #t)]
   [(from/to "rem" "\n")
    (token 'REM
           (string-downcase lexeme)
           #:position (pos lexeme-start)
           #:line (line lexeme-start)
           #:column (col lexeme-start)
           #:span (- (pos lexeme-end)
                     (pos lexeme-start)))]
   [(:or "print" "goto" "end")
    (token (string-downcase lexeme)
           (string-downcase lexeme)
           #:position (pos lexeme-start)
           #:line (line lexeme-start)
           #:column (col lexeme-start)
           #:span (- (pos lexeme-end)
                     (pos lexeme-start)))]
   [(:+ numeric)
    (token 'NUMBER
           (string->number lexeme)
           #:position (pos lexeme-start)
           #:line (line lexeme-start)
           #:column (col lexeme-start)
           #:span (- (pos lexeme-end)
                     (pos lexeme-start)))]
   [(from/to "\"" "\"")
    (token 'STRING
           (trim-ends  "\"" lexeme "\"")
           #:position (pos lexeme-start)
           #:line (line lexeme-start)
           #:column (col lexeme-start)
           #:span (- (pos lexeme-end)
                     (pos lexeme-start)))]))

(define (tokenize ip)
  (port-count-lines! ip)
  (thunk (basic-lexer ip)))
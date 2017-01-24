#lang br
(require brag/support syntax-color/racket-lexer racket/contract
         basic-demo/tokenizer sugar/coerce)

;(values lexeme 'parenthesis '|(| (pos lexeme-start) (pos lexeme-end))

(define (color-basic ip)
  (define postok ((tokenize ip)))
  (define tok-or-str (position-token-token postok))
  (define type (if (string? tok-or-str)
                   'string
                   (token-struct-type tok-or-str)))
  (define val (if (string? tok-or-str)
                  tok-or-str
                  (->string (or (token-struct-val tok-or-str) ""))))
  (values val
          (caseq type
                 [(WHITE) 'white-space]
                 [(COMMENT) 'comment]
                 [(NUMBER) 'constant]
                 [(STRING) 'string]
                 [else 'no-color])
          #f
          (position-offset (position-token-start-pos postok))
          (position-offset (position-token-end-pos postok))))
  

(provide
 (contract-out
  [color-basic
   (input-port? . -> . (values
                        (or/c string? eof-object?)
                        symbol?
                        (or/c symbol? #f)
                        (or/c exact-positive-integer? #f)
                        (or/c exact-positive-integer? #f)))]))

(module+ main
  (define p (open-input-string #<<HERE

10 rem foo
20 rem foo
30 let x = 42
HERE
                               ))
  (color-basic p)
  (color-basic p)
  (color-basic p))
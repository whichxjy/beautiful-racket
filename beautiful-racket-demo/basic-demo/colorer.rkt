#lang br
(require brag/support syntax-color/racket-lexer racket/contract
         basic-demo/tokenizer sugar/coerce)

(define (color-basic ip)
  (define postok ((tokenize ip)))
  (define tok (position-token-token postok))
  (define-values (type val)
    (cond
      [(eof-object? tok) (values 'eof "")]
      [(string? tok) (values 'string tok)]
      [else (values (token-struct-type tok)
                    (format "~a" (token-struct-val tok)))]))
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
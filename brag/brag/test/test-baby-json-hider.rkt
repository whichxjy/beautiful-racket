#lang racket/base
(require brag/examples/baby-json-hider
         brag/support
         rackunit)

(define parse-result (parse (list "{" 
                                  (token 'ID "message")
                                  ":"
                                  (token 'STRING "'hello world'")
                                  "}")))
(check-equal? (syntax->datum parse-result) '(my:json (":")))

(define syntaxed-colon-parens (cadr (syntax->list parse-result)))
(check-equal? (syntax->datum (syntax-property syntaxed-colon-parens 'my:kvpair)) 'my:kvpair)

(check-equal? 
 (syntax->datum
  (parse "[[[{}]],[],[[{}]]]"))
 '(my:json (my:array #\[ (my:json (my:array #\[ (my:json (my:array #\[ (my:json) #\])) #\])) #\, (my:json (my:array #\[ #\])) #\, (my:json (my:array #\[ (my:json (my:array #\[ (my:json) #\])) #\])) #\])))

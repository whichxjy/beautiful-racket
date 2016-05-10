#lang racket/base
(require brag/examples/baby-json-hider
         brag/support
         rackunit)

(check-equal?
 (syntax->datum
  (parse (list "{" 
               (token 'ID "message")
               ":"
               (token 'STRING "'hello world'")
               "}")))
 '(json ":"))


(check-equal? 
 (syntax->datum
  (parse "[[[{}]],[],[[{}]]]"))
 '(json (array #\[ (json (array #\[ (json (array #\[ (json) #\])) #\])) #\, (json (array #\[ #\])) #\, (json (array #\[ (json (array #\[ (json) #\])) #\])) #\])))

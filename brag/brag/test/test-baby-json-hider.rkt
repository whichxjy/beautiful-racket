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
 '(json (object (":"))))


(check-equal? 
 (syntax->datum
  (parse "[[[{}]],[],[[{}]]]"))
 '(json (array #\[ (json (array #\[ (json (array #\[ (json (object)) #\])) #\])) #\, (json (array #\[ #\])) #\, (json (array #\[ (json (array #\[ (json (object )) #\])) #\])) #\])))

 
               
               

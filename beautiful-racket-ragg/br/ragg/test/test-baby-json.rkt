#lang racket/base
(require br/ragg/examples/baby-json
         br/ragg/support
         rackunit)

(check-equal?
 (syntax->datum
  (parse (list "{" 
               (token 'ID "message")
               ":"
               (token 'STRING "'hello world'")
               "}")))
 '(json (object "{"
                (kvpair "message" ":" (json (string "'hello world'")))
                "}")))

(require sugar/debug)
(syntax-property (report (cadr (syntax->list (cadr (syntax->list (parse (list "{" 
               (token 'ID "message")
               ":"
               (token 'STRING "'hello world'")
               "}"))))))) 'foo)

#;(check-equal? 
 (syntax->datum
  (parse "[[[{}]],[],[[{}]]]"))
 '(json (array #\[ (json (array #\[ (json (array #\[ (json (object #\{ #\})) #\])) #\])) #\, (json (array #\[ #\])) #\, (json (array #\[ (json (array #\[ (json (object #\{ #\})) #\])) #\])) #\])))

 
               
               

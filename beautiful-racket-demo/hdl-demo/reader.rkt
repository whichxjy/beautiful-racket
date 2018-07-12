#lang br
(require "parser.rkt" "tokenizer.rkt")
(provide read-syntax)
(define (read-syntax src ip)
  (strip-context
   (with-syntax ([PT (parse src (tokenize ip))])
     #'(module hdl-mod hdl-demo/expander
         PT))))

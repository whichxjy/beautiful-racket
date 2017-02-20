#lang br
(require "parser.rkt" "tokenizer.rkt")
(provide read-syntax)
(define (read-syntax source-path input-port)
  #`(module hdl-mod hdl-demo/expander
      #,(parse source-path (tokenize input-port))))

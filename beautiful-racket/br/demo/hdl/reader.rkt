#lang br
(require br/reader-utils "parser.rkt" "tokenizer.rkt")

(provide read-syntax)
(define (read-syntax source-path input-port)
  (strip-context #`(module hdl-mod br/demo/hdl/expander
                     #,(parse source-path (tokenize input-port)))))

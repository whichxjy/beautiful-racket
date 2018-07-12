#lang br

(module reader br
  (require "parser.rkt" "tokenizer.rkt")
  (provide read-syntax)
  (define (read-syntax source-path port)
    (define-values (line col pos) (port-next-location port))
    (define port+newline (input-port-append #f port (open-input-string "\n")))
    (port-count-lines! port+newline)
    (set-port-next-location! port+newline line col pos)
    (strip-context
     (with-syntax ([PT (parse source-path (make-tokenizer port+newline))])
       #'(module hdl-mod hdl-tst-demo/expander
           PT)))))

#lang br

(module reader br
  (require br/reader-utils "parser.rkt" "tokenizer.rkt")
  (define-read-and-read-syntax (source-path input-port)
    #`(module hdl-mod hdl-tst-demo/expander
        #,(parse source-path (tokenize input-port)))))

#lang br

(module reader br
  (require br/reader-utils "tst/parser.rkt" "tst/tokenizer.rkt")
  (define-read-and-read-syntax (source-path input-port)
    #`(module hdl-mod br/demo/hdl/tst/expander
        #,(parse source-path (tokenize input-port)))))

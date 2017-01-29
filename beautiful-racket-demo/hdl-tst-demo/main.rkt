#lang br

(module reader br
  (require br/reader-utils "parser.rkt" "tokenizer.rkt")
  (define-read-and-read-syntax (source-path port)
    (define-values (line col pos) (port-next-location port))
    (define port+newline (input-port-append #f port (open-input-string "\n")))
    (port-count-lines! port+newline)
    (set-port-next-location! port+newline line col pos)
    #`(module hdl-mod hdl-tst-demo/expander
        #,(parse source-path (make-tokenizer port+newline)))))

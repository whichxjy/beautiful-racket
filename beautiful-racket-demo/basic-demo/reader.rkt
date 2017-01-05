#lang br
(require br/reader-utils "parser.rkt" "tokenizer.rkt")

(define-read-and-read-syntax (source-path input-port)
  #`(module bf-mod basic-demo/expander
      #,(parse source-path (tokenize input-port))))

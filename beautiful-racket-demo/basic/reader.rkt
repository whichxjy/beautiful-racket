#lang br
(require br/reader-utils "parser.rkt" "tokenizer.rkt")

(define-read-and-read-syntax (source-path input-port)
  #`(module bf-mod br/demo/basic/expander
      #,(parse source-path (tokenize input-port))))

#lang br

(module reader br
  (require "tokenizer.rkt" "parser.rkt" syntax/strip-context)
  (provide read-syntax)
  (define (read-syntax src-path src-port)
    (define parsed-stx (parse src-path (tokenize src-port)))
    (define new-ctxt-stx (datum->syntax #f 'new-ctxt))
    (inject-syntax ([#'src-stx (replace-context new-ctxt-stx parsed-stx)])
                   #'(module bf-interpreter br-bf/expander
                       src-stx))))
  
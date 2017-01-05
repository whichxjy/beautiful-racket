#lang br/quicklang
(require "tokenizer.rkt" "parser.rkt" racket/contract)

(define (read-syntax path port)
  (define parse-tree (parse path (tokenize port)))
  (define module-datum `(module jsonic-module jsonic-demo-2/expander
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide (contract-out
          [read-syntax (any/c input-port? . -> . syntax?)]))

#lang br/quicklang
(require "tokenizer.rkt" "parser.rkt" racket/contract)

(define/contract (read-syntax path port)
  (any/c input-port? . -> . syntax?)
  (define parse-tree (parse path (tokenize port)))
  (define module-datum `(module jsonic-module jsonic-demo-2/expander
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)

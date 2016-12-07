#lang br/quicklang
(require "tokenizer.rkt" "parser.rkt")

(define/contract (read-syntax path port)
  (path? input-port? . -> . syntax?)
  (define parse-tree (parse path (tokenize port)))
  (define module-datum `(module jsonic-module jsonic/expander
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)
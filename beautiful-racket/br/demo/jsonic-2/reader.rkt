#lang br/quicklang
(require "tokenizer.rkt" "parser.rkt")
(define (syntax-no-bindings? stx)
  (and (syntax? stx)
       (equal? stx (strip-bindings stx))))
(define/contract (read-syntax path port)
  (path? input-port? . -> . syntax?)
  (define parse-tree (parse path (tokenize port)))
  (define module-datum `(module jsonic-module br/demo/jsonic/expander
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)
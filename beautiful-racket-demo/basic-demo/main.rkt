#lang br/quicklang
(require "parser.rkt" "tokenizer.rkt")

(module+ reader (provide read-syntax))

(define (read-syntax path port)
  (define-values (pline pcol ppos) (port-next-location port))
  (define port+newline
    (input-port-append #f port (open-input-string "\n")))
  (port-count-lines! port+newline)
  (set-port-next-location! port+newline pline pcol ppos)
  (define parse-tree
    (parse path (make-tokenizer port+newline path)))
  (strip-bindings
   #`(module basic-mod basic-demo/expander
       #,parse-tree)))
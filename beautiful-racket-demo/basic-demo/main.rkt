#lang br/quicklang
(require "parser.rkt" "tokenizer.rkt")

(module+ reader (provide read-syntax))

(define (read-syntax path port)
  (define-values (line col pos) (port-next-location port))
  (define port+newline (input-port-append #f port (open-input-string "\n")))
  (port-count-lines! port+newline)
  (set-port-next-location! port+newline line col pos)
  (define parse-tree (parse path (tokenize port+newline)))
  (strip-bindings
   #`(module basic-mod basic-demo/expander
       #,parse-tree)))
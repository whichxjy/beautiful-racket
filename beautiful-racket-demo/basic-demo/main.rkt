#lang br/quicklang
(require "parser.rkt" "tokenizer.rkt" brag/support)

(module+ reader (provide read-syntax))

(define (read-syntax path port)
  (define-values (line col pos) (port-next-location port))
  (define port+newline (input-port-append #f port (open-input-string "\n")))
  (port-count-lines! port+newline)
  (set-port-next-location! port+newline line col pos)
  (with-handlers ([exn:fail:parsing? (λ (exn) (displayln "Sorry!") (raise exn))])
    (define parse-tree (parse path (make-tokenizer port+newline)))
    (strip-bindings
     #`(module basic-mod basic-demo/expander
         #,parse-tree))))
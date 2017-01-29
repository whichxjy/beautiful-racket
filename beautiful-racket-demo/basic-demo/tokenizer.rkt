#lang br
(require "lexer.rkt" brag/support racket/contract)

(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (file-path path)
  (define (next-token) (basic-lexer ip))
  next-token)

(provide
 (contract-out
  [make-tokenizer
   ((input-port?) (path?) . ->* .
                  (-> (or/c eof-object? string? srcloc-token?)))]))
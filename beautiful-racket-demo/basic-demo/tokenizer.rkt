#lang br
(require "lexer.rkt" brag/support racket/contract)

(define (make-tokenizer ip [path #f])
  (port-count-lines! ip)
  (file-path path)
  (define (next-token) (basic-lexer ip))
  next-token)

(provide make-tokenizer)
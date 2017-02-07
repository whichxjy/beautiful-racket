#lang br
(require "goto.rkt" "line.rkt")
(provide b-gosub b-return)

(define return-stack empty)

(define (b-gosub num-expr)
  (let/cc return-cc
    (push! return-stack return-cc)
    (b-goto num-expr)))

(define (b-return)
  (unless (pair? return-stack)
    (raise-line-error "return without gosub"))
  (define top-return-k (pop! return-stack))
  (top-return-k))
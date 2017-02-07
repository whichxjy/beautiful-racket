#lang br
(require "structs.rkt" "line.rkt")
(provide b-goto b-gosub b-return)

(define (b-goto num-expr)
  (raise (change-line-signal num-expr)))

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
#lang racket/base
(require racket/list)
(provide (all-defined-out))

(define (syntax-flatten stx)
  (flatten
   (let loop ([stx stx])
     (let* ([stx-unwrapped (syntax-e stx)]
            [maybe-pair (and (pair? stx-unwrapped) (flatten stx-unwrapped))])
       (if maybe-pair
           (map loop maybe-pair)
           stx)))))
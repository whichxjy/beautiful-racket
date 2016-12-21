#lang racket/base
(require racket/class
         racket/draw)

(provide (all-defined-out))
(define (make-drracket-button . args)
  (define label (or (findf string? args) "untitled"))
  (define bitmap (or (findf (λ(arg) (is-a? arg bitmap%)) args) (make-object bitmap% 16 16)))
  (define callback (or (findf procedure? args) (λ(drr-frame) (void))))
  (define number (or (findf (λ(arg) (or (real? arg) (equal? #f arg))) args) #f))
  (list label bitmap callback number))

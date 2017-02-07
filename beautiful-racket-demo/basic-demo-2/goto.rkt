#lang br
(require "structs.rkt")
(provide b-goto)
(define (b-goto num-expr)
  (raise (change-line-signal num-expr)))
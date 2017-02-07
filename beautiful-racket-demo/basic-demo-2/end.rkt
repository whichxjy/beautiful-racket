#lang br
(require "structs.rkt")
(provide b-end)
(define (b-end)
  (raise (end-program-signal)))
#lang br
(provide (matching-identifiers-out #rx"^b-" (all-defined-out)))

(define (b-rem val) (void))
(define (b-print [val ""]) (displayln val))
(define-macro (b-let ID VAL)
  #'(set! ID VAL))

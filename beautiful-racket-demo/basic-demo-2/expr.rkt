#lang br
(provide (matching-identifiers-out #rx"^b-" (all-defined-out)))

(define (b-sum . nums) (apply + nums))

(define (b-num-expr expr)
  (if (integer? expr) (inexact->exact expr) expr))

(define (b-negative num) (- num))
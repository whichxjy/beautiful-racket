#lang br
(provide (prefix-out Nand- (all-defined-out)))
(require "helper.rkt")

(define-input-bus a)
(define-input-bus b)

(define (out . etc)
  (if (< (+ (a) (b)) 2)
      1
      0))

(module+ test
  (require rackunit)
  (check-equal? (begin (a 0) (b 0) (out)) 1)
  (check-equal? (begin (a 0) (b 1) (out)) 1)
  (check-equal? (begin (a 1) (b 0) (out)) 1)
  (check-equal? (begin (a 1) (b 1) (out)) 0))

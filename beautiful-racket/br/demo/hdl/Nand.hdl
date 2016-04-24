#lang br

(define+provide (Nand #:a a #:b b)
  (if (< (+ a b) 2)
      1
      0))

(module+ test
  (require rackunit)
  (check-equal? (Nand #:a 0 #:b 0) 1)
  (check-equal? (Nand #:a 0 #:b 1) 1)
  (check-equal? (Nand #:a 1 #:b 0) 1)
  (check-equal? (Nand #:a 1 #:b 1) 0))

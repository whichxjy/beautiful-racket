#lang br
(provide (all-defined-out))

(struct Nand (a b out) #:transparent)

(define (make-Nand)
  (Nand a b out))

(define a
  (let ([Nand-a-val 0])
    (λ ([val #f])
      (if val
          (set! Nand-a-val val)
          Nand-a-val))))

(define b
  (let ([Nand-b-val 0])
    (λ ([val #f])
      (if val
          (set! Nand-b-val val)
          Nand-b-val))))


(define (out)
  (if (< (+ (a) (b)) 2)
      1
      0))

(module+ test
  (require rackunit)
  (check-equal? (begin (a 0) (b 0) (out)) 1)
  (check-equal? (begin (a 0) (b 1) (out)) 1)
  (check-equal? (begin (a 1) (b 0) (out)) 1)
  (check-equal? (begin (a 1) (b 1) (out)) 0))

(define n (make-Nand))

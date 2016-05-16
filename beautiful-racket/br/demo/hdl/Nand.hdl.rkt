#lang br

(define Nand-a
  (let ([Nand-a-val 0])
    (λ ([val #f])
      (if val
          (set! Nand-a-val val)
          Nand-a-val))))

(define Nand-b
  (let ([Nand-b-val 0])
    (λ ([val #f])
      (if val
          (set! Nand-b-val val)
          Nand-b-val))))


(define (Nand-out)
  (if (< (+ (Nand-a) (Nand-b)) 2)
      1
      0))

(module+ test
  (require rackunit)
  (check-equal? (begin (Nand-a 0) (Nand-b 0) (Nand-out)) 1)
  (check-equal? (begin (Nand-a 0) (Nand-b 1) (Nand-out)) 1)
  (check-equal? (begin (Nand-a 1) (Nand-b 0) (Nand-out)) 1)
  (check-equal? (begin (Nand-a 1) (Nand-b 1) (Nand-out)) 0))

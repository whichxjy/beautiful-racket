#lang br

(define Nand-in-a
  (let ([Nand-a-val 0])
    (λ ([val #f])
      (if val
          (set! Nand-a-val val)
          Nand-a-val))))

(define Nand-in-b
  (let ([Nand-b-val 0])
    (λ ([val #f])
      (if val
          (set! Nand-b-val val)
          Nand-b-val))))


(define (Nand-out-out)
  (if (< (+ (Nand-in-a) (Nand-in-b)) 2)
      1
      0))

(module+ test
  (require rackunit)
  (check-equal? (begin (Nand-in-a 0) (Nand-in-b 0) (Nand-out-out)) 1)
  (check-equal? (begin (Nand-in-a 0) (Nand-in-b 1) (Nand-out-out)) 1)
  (check-equal? (begin (Nand-in-a 1) (Nand-in-b 0) (Nand-out-out)) 1)
  (check-equal? (begin (Nand-in-a 1) (Nand-in-b 1) (Nand-out-out)) 0))


(struct ins ([a #:auto] [b #:auto]) #:transparent
  #:auto-value (open-input-bytes #""))
(struct outs ([out #:auto]) #:transparent)
(struct Nand (i o) #:transparent)

(define f (Nand (ins) (outs)))
#lang racket
#|

CHIP Not {
          IN in;
          OUT out;
             
             PARTS:
             Nand(a=in, b=in, out=out);
             
             }

|#

(provide (prefix-out Not- (all-defined-out)))

(require "Nand2.hdl.rkt")

(define in
  (let ([in-val 0])
    (λ ([val #f])
      (if val
          (set! in-val val)
          in-val))))

(define n (make-Nand))
(define (out) (begin ((Nand-a n) (in)) ((Nand-b n) (in)) ((Nand-out n))))
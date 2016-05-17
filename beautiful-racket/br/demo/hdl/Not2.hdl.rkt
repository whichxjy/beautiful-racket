#lang racket
#|

CHIP Not {
          IN in;
          OUT out, outb;
             
             PARTS:
             ;; each part has only as many args as wires in that part
             Nand(a=in, b=in, out=nand-out); 
             Fanout(in=nand-out, outa=out, outb=outb);
             
             }

|#

(require "helper.rkt" "helper-macro.rkt" (for-syntax "helper.rkt" racket/syntax racket/list))

;; IN and OUT spec becomes provide spec, prefixed with chip name
(provide (prefix-out Not- (combine-out in out outb))) 

;; all IN and OUT pins are functions.

(define in (make-input)) ; all inputs are made from the same function that holds state like a parameter.

;; all outputs are computed at runtime.
(require "Nand2.hdl.rkt" (for-syntax "Nand2.hdl.rkt"))
(handle-part Nand2 [a in] [b in] [out nand-out])
(require "Fanout.hdl.rkt" (for-syntax "Fanout.hdl.rkt"))
(handle-part Fanout [in nand-out] [outa out] [outb outb])
;(handle-require Fanout [in nand-out] [outa out] [outb outb])

(module+ test
  (require rackunit)
  (in 1)
  (check-equal? (out) 0)
  (in 0)
  (check-equal? (out) 1))
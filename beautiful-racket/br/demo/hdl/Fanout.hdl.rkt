#lang br
(provide (prefix-out Fanout- (all-defined-out)))
(require "helper.rkt")
(define in (make-input))


(define (outa)
  (in))

(define (outb)
  (in))

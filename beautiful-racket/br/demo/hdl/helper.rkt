#lang racket/base
(provide (all-defined-out))

(define-values (input-wire input-wire? input-wire-get)
  (make-impersonator-property 'input-wire))

(define (make-input)
  (impersonate-procedure 
   (let ([val #f])
     (λ ([arg #f])
       (if arg
           (set! val arg)
           val)))
   #f input-wire #t))

(module+ test
  (require rackunit)
  (define in-wire (make-input))
  (define other (λ () (+ 2 2)))
  (check-true (input-wire? in-wire))
  (check-false (input-wire? other)))
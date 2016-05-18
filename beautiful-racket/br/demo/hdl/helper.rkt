#lang racket/base
(provide (all-defined-out))

(define-values (input-wire input-wire? input-wire-get)
  (make-impersonator-property 'input-wire))

(define (make-input [max-length 16])
  (impersonate-procedure 
   (let ([max-length max-length]
         [val 0])
     (case-lambda
       [() val]
       [(bit)
        (when (and bit (>= bit max-length))
          (raise-argument-error 'make-input (format "bit index too large for bit length ~a" max-length) bit))
        (if (bitwise-bit-set? val (or bit 0)) 1 0)]
       [(bit arg)
        (when (and bit (>= bit max-length))
          (raise-argument-error 'make-input (format "bit index too large for bit length ~a" max-length) bit))
        (when (and arg (> arg (expt 2 max-length)))
          (raise-argument-error 'make-input (format "value too large for bit length ~a" max-length) arg))
        (cond
          [(and bit arg) (set! val (bitwise-ior val (expt 2 bit)))]
          [else (set! val arg)])])) ;; aka (and arg (not bit))
   #f input-wire #t))

(module+ test
  (require rackunit)
  (define in-wire (make-input))
  (define other (λ () (+ 2 2)))
  (check-true (input-wire? in-wire))
  (check-false (input-wire? other))
  
  (define x (make-input 4))
  (check-equal? (x) 0)
  (x #f 12)
  (check-equal? (x) 12)
  (x #f 0)
  (check-equal? (x) 0)
  (x 3 1)
  (check-equal? (x) 8)
  (x 2 1)
  (check-equal? (x) 12)
  (check-equal? (x 3) 1)
  (check-equal? (x 2) 1)
  (check-equal? (x 1) 0)
  (check-equal? (x 0) 0)
  
  (check-exn exn:fail? (λ () (x #f 32)))
  (check-exn exn:fail? (λ () (x 22 1)))
  )

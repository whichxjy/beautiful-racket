#lang racket/base
(require racket/match racket/list)
(provide (all-defined-out))

(define (bus-range start [finish start])
  (range start (add1 finish)))

(define-values (input-wire input-wire? input-wire-get)
  (make-impersonator-property 'input-wire))

(define (make-bus bus-name [width 1])
  (impersonate-procedure 
   (procedure-rename
    (let ([bus-width width]
          [bus-val 0])
      (define (do-arg-check arg)
        (when (and arg (> arg (expt 2 bus-width)))
          (raise-argument-error bus-name (format "value that fits into bus width ~a (= under ~a)" bus-width (expt 2 bus-width)) arg)))
      (case-lambda
        [() bus-val]
        [(arg)
         (do-arg-check arg)
         (set! bus-val arg)]
        [(bus-bits arg)
         (unless (and (< (first bus-bits) bus-width) (< (last bus-bits) bus-width))
           (raise-argument-error bus-name (format "bus bit spec less than bus width ~a" bus-width) bus-bits))
         (do-arg-check arg)
         (set! bus-val arg)])) bus-name)
   #f input-wire #t))

(module+ test
  (require rackunit)
  (define in-wire (make-bus 'in-wire))
  (define other (λ () (+ 2 2)))
  (check-true (input-wire? in-wire))
  (check-false (input-wire? other))
  
  (define x (make-bus 'x 4))
  (check-equal? (x) 0)
  (x 12)
  (check-equal? (x) 12)
  (x 0)
  (check-equal? (x) 0)
  (x 12)
  (check-equal? (x) 12)
  (check-exn exn:fail? (λ () (x 32)))
  )

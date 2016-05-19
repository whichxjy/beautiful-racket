#lang racket/base
(require racket/match racket/list)
(provide (all-defined-out))

(module+ test
  (require rackunit))

(define (bitwise-bit-set x bit)
  (if (not (bitwise-bit-set? x bit))
      (bitwise-ior x (expt 2 bit))
      x))

(define (bitwise-bit-unset x bit)
  (if (bitwise-bit-set? x bit)
      (bitwise-and x (bitwise-not (expt 2 bit)))
      x))

(module+ test
  (define x-bitset (string->number "1011" 2)) ; decimal 11
  
  (check-true (bitwise-bit-set? x-bitset 0))
  (check-true (bitwise-bit-set? x-bitset 1))
  (check-false (bitwise-bit-set? x-bitset 2))
  (check-true (bitwise-bit-set? x-bitset 3))
  
  (set! x-bitset (bitwise-bit-set x-bitset 2))
  (check-true (bitwise-bit-set? x-bitset 2))
  
  (set! x-bitset (bitwise-bit-unset x-bitset 2))
  (check-false (bitwise-bit-set? x-bitset 2)))

(define (bus-range start [finish start])
  (range start (add1 finish)))

(define-values (input-bus input-bus? input-bus-get)
  (make-impersonator-property 'input-bus))

(define (integer->bitvals int width)
  (reverse (for/list ([i (in-range width)])
                     (bitwise-bit-field int i (add1 i)))))

(define max-bus-width 16)

(define (check-bit-against-width bus-name bit width)
  (unless (< bit width)
    (raise-argument-error bus-name (format "bit less than bus width ~a" width) bit)))

(require sugar/debug)
(define (make-input-bus bus-name [width 1])
  (impersonate-procedure
   (procedure-rename
    (let ([bus-width width]
          [bus-val 0])
      (unless (<= bus-width max-bus-width)
        (raise-argument-error bus-name (format "bus width <= max width ~a" max-bus-width) bus-width))
      (define (check-val-against-width val width)
        (when (and val (> val (sub1 (expt 2 width))))
          (raise-argument-error bus-name
                                (format "~a-bit value (0 to ~a inclusive)" width (sub1 (expt 2 width))) val)))
      (define func
        (case-lambda
          [() bus-val]
          [(new-val-in)
           (define new-val (cond
                             [(boolean? new-val-in)
                               (if new-val-in (sub1 (expt 2 bus-width)) 0)]
                             [(or (input-bus? new-val-in) (output-bus? new-val-in)) (new-val-in)]
                             [else new-val-in]))
           (check-val-against-width new-val bus-width)
           (set! bus-val new-val)]
          [(bit new-val) (func bit bit new-val)]
          [(first-bit last-bit new-val-in)
           (define bit-range-width (add1 (- last-bit first-bit)))
           (define new-val (cond
                             [(boolean? new-val-in)
                               (if new-val-in (sub1 (expt 2 bit-range-width)) 0)]
                             [(or (input-bus? new-val-in) (output-bus? new-val-in)) (new-val-in)]
                             [else new-val-in]))
           (unless (<= first-bit last-bit)
             (raise-argument-error bus-name (format "last bit greater than or equal to first bit ~a" first-bit) last-bit))
           (check-bit-against-width bus-name first-bit bus-width)
           (check-bit-against-width bus-name last-bit bus-width)
           (check-val-against-width new-val bit-range-width)
           (for ([bit (in-range first-bit (add1 last-bit))]
                 [new-bit-val (in-list (integer->bitvals new-val bit-range-width))])
                (set! bus-val ((if (= 1 new-bit-val) bitwise-bit-set bitwise-bit-unset) bus-val bit)))]))
      func)
    bus-name)
   #f input-bus #t))

(define-syntax-rule (define-input-bus id arg ...)
  (define id (make-input-bus 'id arg ...)))

(module+ test
  (define-input-bus in-bus)
  (define other (λ () (+ 2 2)))
  (check-true (input-bus? in-bus))
  (check-false (input-bus? other))
  
  (define-input-bus ib 4)
  (check-exn exn:fail? (λ () (define-input-bus ib 17) ib)) ; exceeds 16-bit width
  (check-equal? (ib) 0)
  (ib 11) ; set whole value
  (check-exn exn:fail? (λ () (ib #b11111))) ; overflow
  (ib 2 1) ; set bit
  (check-equal? (ib) #b1111)  
  (ib 0 #b0) ; set bit
  (ib 1 #b0) ; set bit
  (ib 2 #b0) ; set bit
  (check-equal? (ib) #b1000)
  (check-exn exn:fail? (λ () (ib 5 1 #b0))) ; last index smaller than first
  (check-exn exn:fail? (λ () (ib 1 300 #b0))) ; overlarge bit index
  (check-exn exn:fail? (λ () (ib 300 500 #b0))) ; overlarge bit index
  (check-exn exn:fail? (λ () (ib 1 #b11111))) ; overflow value
  (ib 0)
  (ib 1 2 #b11) 
  (check-equal? (ib) #b0110)
  (ib 3 3 #b1)
  (ib 0 0 #b1)
  (check-equal? (ib) #b1111)
  (check-exn exn:fail? (λ () (ib 0 300 #b0))) ; overlarge bit index
  (check-exn exn:fail? (λ () (ib 1 1 #b11111))) ; overflow value
  (ib 0)
  (ib 1 2 #t) ; using #t to fill certain bits
  (check-equal? (ib) #b0110)
  (ib 2 2 #f) ; using #f to fill certain bits
  (check-equal? (ib) #b0010)
  (ib 0)
  (ib #t) ; using #t to fill all bits
  (check-equal? (ib) #b1111)
  (ib #f) ; using #f to fill all bits
  (check-equal? (ib) #b0000)
  (define-input-bus ib2 4)
  (check-exn exn:fail? (λ () (ib2 16))) ; overflow value
  (ib2 #b1100)
  (ib ib2) ; using bus as input value
  (check-equal? (ib) (ib2))
  )


(define-values (output-bus output-bus? output-bus-get)
  (make-impersonator-property 'output-bus))

(define (make-output-bus bus-name thunk [width 1])
  (impersonate-procedure
   (procedure-rename
    (let ([bus-width width])
      (unless (<= bus-width max-bus-width)
        (raise-argument-error bus-name (format "bus width <= max width ~a" max-bus-width) bus-width))
      (define func
        (case-lambda
          [() (func 0 (sub1 bus-width))]
          [(bit) (func bit bit)]
          [(first-bit last-bit)
           (unless (<= first-bit last-bit)
             (raise-argument-error bus-name (format "last bit greater than or equal to first bit ~a" first-bit) last-bit))
           (check-bit-against-width bus-name first-bit bus-width)
           (check-bit-against-width bus-name last-bit bus-width)
           (bitwise-bit-field (thunk) first-bit (add1 last-bit))]))
      func)
    bus-name)
   #f output-bus #t))

(define-syntax-rule (define-output-bus id thunk arg ...)
  (define id (make-output-bus 'id thunk arg ...)))

(module+ test
  (define-output-bus ob (λ () #b0110) 4)
  (check-exn exn:fail? (λ () (define-input-bus ob (λ () #b0110) 17) ob)) ; exceeds 16-bit width
  (check-equal? (ob) #b0110)
  (check-equal? (ob 0) #b0)
  (check-equal? (ob 1) #b1)
  (check-equal? (ob 2) #b1)
  (check-equal? (ob 3) #b0)
  (check-exn exn:fail? (λ () (ob 5))) ; exceeds bus width
  (check-equal? (ob 0 1) #b10)
  (check-equal? (ob 1 2) #b11)
  (check-equal? (ob 2 3) #b01)
  (check-exn exn:fail? (λ () (ob 3 2))) ; inverted bus spec
  (check-exn exn:fail? (λ () (ob 5 10))) ; exceeds bus width
  )

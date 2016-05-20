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
(define default-bus-width 1)


(define (check-bit-against-width bus-name bit width)
  (unless (< bit width)
    (raise-argument-error bus-name (format "bit less than bus width ~a" width) bit)))

(define (check-val-against-width bus-name val width)
  (when (and val (> val (sub1 (expt 2 width))))
    (raise-argument-error bus-name
                          (format "~a-bit value (0 to ~a inclusive)" width (sub1 (expt 2 width))) val)))

(require sugar/debug)
(define (make-input-bus bus-name [width 1])
  (impersonate-procedure
   (procedure-rename
    (let ([bus-width width]
          [bus-val 0])
      (unless (<= bus-width max-bus-width)
        (raise-argument-error bus-name (format "bus width <= max width ~a" max-bus-width) bus-width))
      
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


(require (for-syntax racket/base racket/syntax))
(define-syntax (define-input-bus stx)
  (syntax-case stx ()
    [(macro-name id)
     #'(macro-name id void default-bus-width)]
    [(macro-name id width)
     #'(macro-name id void width)]
    [(macro-name id thunk width)
     (with-syntax ([id-write (format-id #'id "~a-write" #'id)])
       #'(begin
           (define-output-bus id thunk width)
          ))]))

#;(module+ test
    (define-input-bus in-bus)
    (define other (λ () (+ 2 2)))
    (check-true (input-bus? in-bus))
    (check-false (input-bus? other))
    
    (define-input-bus ib 4)
    (check-exn exn:fail? (λ () (define-input-bus ib 17) ib)) ; exceeds 16-bit width
    (check-equal? (ib-read) 0)
    (ib-write 11) ; set whole value
    (check-exn exn:fail? (λ () (ib-write #b11111))) ; overflow
    (ib-write 2 1) ; set bit
    (check-equal? (ib) #b1111)  
    (ib-write 0 #b0) ; set bit
    (ib-write 1 #b0) ; set bit
    (ib-write 2 #b0) ; set bit
    (check-equal? (ib-read) #b1000)
    (check-exn exn:fail? (λ () (ib-write 5 1 #b0))) ; last index smaller than first
    (check-exn exn:fail? (λ () (ib-write 1 300 #b0))) ; overlarge bit index
    (check-exn exn:fail? (λ () (ib-write 300 500 #b0))) ; overlarge bit index
    (check-exn exn:fail? (λ () (ib-write 1 #b11111))) ; overflow value
    (ib-write 0)
    (ib-write 1 2 #b11) 
    (check-equal? (ib-read) #b0110)
    (ib-write 3 3 #b1)
    (ib-write 0 0 #b1)
    (check-equal? (ib-read) #b1111)
    (check-exn exn:fail? (λ () (ib-write 0 300 #b0))) ; overlarge bit index
    (check-exn exn:fail? (λ () (ib-write 1 1 #b11111))) ; overflow value
    (ib-write 0)
    (ib-write 1 2 #t) ; using #t to fill certain bits
    (check-equal? (ib-read) #b0110)
    (ib-write 2 2 #f) ; using #f to fill certain bits
    (check-equal? (ib-read) #b0010)
    (ib-write 0)
    (ib-write #t) ; using #t to fill all bits
    (check-equal? (ib-read) #b1111)
    (ib-write #f) ; using #f to fill all bits
    (check-equal? (ib-read) #b0000)
    (define-input-bus ib2 4)
    (check-exn exn:fail? (λ () (ib2-write 16))) ; overflow value
    (ib2-write #b1100)
    (ib-write (ib2-read)) ; using bus as input value
    (check-equal? (ib-read) (ib2))
    )


(define-values (output-bus output-bus? output-bus-get)
  (make-impersonator-property 'output-bus))

(define (make-read-bus bus-name thunk bus-width)
  (unless (<= bus-width max-bus-width)
    (raise-argument-error bus-name (format "bus width <= max width ~a" max-bus-width) bus-width))
  (impersonate-procedure (procedure-rename thunk bus-name) #f output-bus #t))

(define (make-bus-reader reader-name id-val thunk width)
  (define bus-reader-func
    (case-lambda
      [() (bus-reader-func 0 (sub1 width))]
      [(bit) (bus-reader-func bit bit)]
      [(first-bit last-bit)
       (unless (<= first-bit last-bit)
         (raise-argument-error reader-name (format "last bit greater than or equal to first bit ~a" first-bit) last-bit))
       (check-bit-against-width reader-name first-bit width)
       (check-bit-against-width reader-name last-bit width)
       (set! id-val (thunk))
       (bitwise-bit-field id-val first-bit (add1 last-bit))]))
  bus-reader-func)

(define (make-bus-writer writer-name id width)
  (define bus-writer-func
    (case-lambda
      [() (raise-argument-error writer-name "new value" empty)]
      [(new-val-in)
       (define new-val (cond
                         [(boolean? new-val-in)
                          (if new-val-in (sub1 (expt 2 width)) 0)]
                         [(or (input-bus? new-val-in) (output-bus? new-val-in)) (new-val-in)]
                         [else new-val-in]))
       (check-val-against-width 'id new-val width)
       (set! id new-val)]
      [(bit new-val) (bus-writer-func bit bit new-val)]
      [(first-bit last-bit new-val-in)
       (define bit-range-width (add1 (- last-bit first-bit)))
       (define new-val (cond
                         [(boolean? new-val-in)
                          (if new-val-in (sub1 (expt 2 bit-range-width)) 0)]
                         [(or (input-bus? new-val-in) (output-bus? new-val-in)) (new-val-in)]
                         [else new-val-in]))
       (unless (<= first-bit last-bit)
         (raise-argument-error writer-name (format "last bit greater than or equal to first bit ~a" first-bit) last-bit))
       (check-bit-against-width writer-name first-bit width)
       (check-bit-against-width writer-name last-bit width)
       (check-val-against-width new-val bit-range-width)
       (for ([bit (in-range first-bit (add1 last-bit))]
             [new-bit-val (in-list (integer->bitvals new-val bit-range-width))])
            (set! id ((if (= 1 new-bit-val) bitwise-bit-set bitwise-bit-unset) id bit)))]))
  bus-writer-func)

(define-syntax (define-output-bus stx)
  (syntax-case stx ()
    [(macro-name id thunk)
     #'(macro-name id thunk default-bus-width)]
    [(macro-name id thunk width)
     (with-syntax ([id-val (format-id #'id "~a-val" #'id)]
                   [id-read (format-id #'id "~a-read" #'id)]
                   [id-write (format-id #'id "~a-write" #'id)])
       #'(begin
           (define id-val 0)
           (define id-read (make-bus-reader 'id-read id-val thunk width))
           (define id (make-read-bus 'id id-read width))
           (define id-write (make-bus-writer 'id-write id width))))]))


(module+ test
  (define-output-bus ob (λ () #b0110) 4)
  (check-exn exn:fail? (λ () (define-output-bus ob (λ () #b0110) 17) ob)) ; exceeds 16-bit width
  (check-equal? (ob-read) #b0110)
  (check-equal? (ob-read 0) #b0)
  (check-equal? (ob-read 1) #b1)
  (check-equal? (ob-read 2) #b1)
  (check-equal? (ob-read 3) #b0)
  (check-exn exn:fail? (λ () (ob-read 5))) ; exceeds bus width
  (check-equal? (ob-read 0 1) #b10)
  (check-equal? (ob-read 1 2) #b11)
  (check-equal? (ob-read 2 3) #b01)
  (check-exn exn:fail? (λ () (ob-read 3 2))) ; inverted bus spec
  (check-exn exn:fail? (λ () (ob-read 5 10))) ; exceeds bus width
  )

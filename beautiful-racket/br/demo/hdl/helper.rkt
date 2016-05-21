#lang racket/base
(require (for-syntax racket/base racket/syntax) racket/splicing)
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


(define (make-bus-reader reader-name width)
  (define bus-reader-func
    (case-lambda
      [(id-thunk-val) (bus-reader-func id-thunk-val 0 (sub1 width))]
      [(id-thunk-val bit) (bus-reader-func id-thunk-val bit bit)]
      [(id-thunk-val first-bit last-bit)
       (unless (<= first-bit last-bit)
         (raise-argument-error reader-name (format "last bit greater than or equal to first bit ~a" first-bit) last-bit))
       (check-bit-against-width reader-name first-bit width)
       (check-bit-against-width reader-name last-bit width)
       (bitwise-bit-field id-thunk-val first-bit (add1 last-bit))]))
  (procedure-rename bus-reader-func reader-name))

(define (make-bus-writer writer-name width)
  (define bus-writer-func
    (case-lambda
      [(id-thunk-val) (raise-argument-error writer-name "new value" empty)]
      [(id-thunk-val new-val-in)
       (define new-val (cond
                         [(boolean? new-val-in)
                          (if new-val-in (sub1 (expt 2 width)) 0)]
                         [(or (input-bus? new-val-in) (output-bus? new-val-in)) (new-val-in)]
                         [else new-val-in]))
       (check-val-against-width writer-name new-val width)
       new-val]
      [(id-thunk-val bit new-val) (bus-writer-func id-thunk-val bit bit new-val)]
      [(id-thunk-val first-bit last-bit new-val-in)
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
       (check-val-against-width writer-name new-val bit-range-width)
       (define last-val
         (for/fold ([val id-thunk-val])
                   ([bit (in-range first-bit (add1 last-bit))]
                    [new-bit-val (in-list (integer->bitvals new-val bit-range-width))])
           ((if (= 1 new-bit-val) bitwise-bit-set bitwise-bit-unset) val bit)))
       last-val]))
  bus-writer-func)


#|

base bus:
+ can read all, or bits
+ every read invokes a thunk
|#


(define-values (bus bus? bus-get)
  (make-impersonator-property 'bus))

(define-syntax (define-base-bus stx)
  (syntax-case stx ()
    [(macro-name id thunk)
     #'(macro-name id thunk default-bus-width)]
    [(macro-name id thunk bus-width-in)
     (with-syntax ([id-thunk (format-id #'id "~a-val" #'id)]
                   [bus-type (or (syntax-property stx 'impersonate) #'bus)])
       #`(splicing-let ([id-thunk thunk]
                        [bus-width bus-width-in])
           (define id
             (begin
               (unless (<= bus-width max-bus-width)
                 (raise-argument-error 'id (format "bus width <= max width ~a" max-bus-width) bus-width))
               (impersonate-procedure
                (let ([reader (make-bus-reader 'id bus-width)])
                  (procedure-rename (λ args (apply reader (id-thunk) args)) (string->symbol (format "~a, a bus of width ~a" 'id bus-width))))
                #f bus-type #t)))
           #,(when (syntax-property stx 'writer)
               (with-syntax ([id-write (format-id #'id "~a-write" #'id)])
                 #'(define id-write
                     (let ([writer (make-bus-writer 'id-write bus-width)])
                       (λ args
                         (define result (apply writer (id-thunk) args))
                         (set! id-thunk (λ () result)))))))))]))


(module+ test
  (define-base-bus bb (λ () #b0110) 4)
  (check-true (bus? bb))
  (check-false (input-bus? bb))
  (check-false (output-bus? bb))
  (check-exn exn:fail? (λ () (define-base-bus bb (λ () #b0110) 17) bb)) ; exceeds 16-bit width
  (check-equal? (bb) #b0110)
  (check-equal? (bb 0) #b0)
  (check-equal? (bb 1) #b1)
  (check-equal? (bb 2) #b1)
  (check-equal? (bb 3) #b0)
  (check-exn exn:fail? (λ () (bb 5))) ; exceeds bus width
  (check-equal? (bb 0 1) #b10)
  (check-equal? (bb 1 2) #b11)
  (check-equal? (bb 2 3) #b01)
  (check-exn exn:fail? (λ () (bb 3 2))) ; inverted bus spec
  (check-exn exn:fail? (λ () (bb 5 10))) ; exceeds bus width
  )


#|
output bus:
+ thunk is a runtime computation
+ cannot write
|#

(define-values (output-bus output-bus? output-bus-get)
  (make-impersonator-property 'output-bus))

(define-syntax (define-output-bus stx)
  (syntax-case stx ()
    [(_ . args)
     (syntax-property #'(define-base-bus . args) 'impersonate #'output-bus)]))

(module+ test
  (define-output-bus ob (λ () #b0110) 4)
  (check-false (bus? ob))
  (check-false (input-bus? ob))
  (check-true (output-bus? ob))
  (check-exn exn:fail? (λ () (define-base-bus ob (λ () #b0110) 17) ob)) ; exceeds 16-bit width
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

#|
input bus:
+ thunk returns a constant
+ identifies itself as input bus
+ can write all, or bits
|#

(define-values (input-bus input-bus? input-bus-get)
  (make-impersonator-property 'input-bus))

(define-syntax (define-input-bus stx)
  (syntax-case stx ()
    [(macro-name id)
     #'(macro-name id default-bus-width)]
    [(macro-name id bus-width)
     (syntax-property
      (syntax-property
       #'(define-base-bus id (λ () 0) bus-width)
       'impersonate #'input-bus)
      'writer #t)]))

(module+ test
  (define-input-bus ib 4)
  (check-false (bus? ib))
  (check-false (output-bus? ib))
  (check-true (input-bus? ib))
  (check-exn exn:fail? (λ () (define-input-bus ib 17) ib)) ; exceeds 16-bit width
  (check-equal? (ib) 0)
  (ib-write 11) ; set whole value
  (check-equal? (ib) 11)
  (check-exn exn:fail? (λ () (ib-write #b11111))) ; overflow
  (ib-write 2 1) ; set bit
  (check-equal? (ib) #b1111)  
  (ib-write 0 #b0) ; set bit
  (ib-write 1 #b0) ; set bit
  (ib-write 2 #b0) ; set bit
  (check-equal? (ib) #b1000)
  (check-exn exn:fail? (λ () (ib-write 5 1 #b0))) ; last index smaller than first
  (check-exn exn:fail? (λ () (ib-write 1 300 #b0))) ; overlarge bit index
  (check-exn exn:fail? (λ () (ib-write 300 500 #b0))) ; overlarge bit index
  (check-exn exn:fail? (λ () (ib-write 1 #b11111))) ; overflow value
  (ib-write 0)
  (ib-write 1 2 #b11) 
  (check-equal? (ib) #b0110)
  (ib-write 3 3 #b1)
  (ib-write 0 0 #b1)
  (check-equal? (ib) #b1111)
  (check-exn exn:fail? (λ () (ib-write 0 300 #b0))) ; overlarge bit index
  (check-exn exn:fail? (λ () (ib-write 1 1 #b11111))) ; overflow value
  (ib-write 0)
  (ib-write 1 2 #t) ; using #t to fill certain bits
  (check-equal? (ib) #b0110)
  (ib-write 2 2 #f) ; using #f to fill certain bits
  (check-equal? (ib) #b0010)
  (ib-write 0)
  (ib-write #t) ; using #t to fill all bits
  (check-equal? (ib) #b1111)
  (ib-write #f) ; using #f to fill all bits
  (check-equal? (ib) #b0000)
  (define-input-bus ib2 4)
  (check-exn exn:fail? (λ () (ib2-write 16))) ; overflow value
  (ib2-write #b1100)
  (ib-write ib2) ; using bus as input value
  (check-equal? (ib) (ib2))
  )
  
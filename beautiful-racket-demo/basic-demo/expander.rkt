#lang br/quicklang

(define-macro (b-module-begin (b-program LINE ...))
  (with-pattern ([(LINE-NUM ...)
                  (filter-stx-prop 'b-line-number (syntax-flatten #'(LINE ...)))]
                 [(LINE-ID ...) (syntax-map (λ (stx) (prefix-id "line-" stx)) #'(LINE-NUM ...))])
    #'(#%module-begin
       LINE ...
       (define line-table (apply hasheqv (append (list LINE-NUM LINE-ID) ...)))
       (run line-table))))
(provide (rename-out [b-module-begin #%module-begin]))

(define-macro (b-line LINE-NUMBER STATEMENT)
  (with-pattern ([LINE-NUMBER-ID (prefix-id "line-" #'LINE-NUMBER
                                            #:source #'LINE-NUMBER)]
                 [ORIG-LOC caller-stx])
    (syntax/loc caller-stx (define (LINE-NUMBER-ID #:srcloc? [srcloc #f])
                             (if srcloc
                                 (syntax-srcloc #'ORIG-LOC)
                                 STATEMENT)))))

(define (b-statement stmt) stmt)
(define (b-rem str) #f)
(define (b-print str) (displayln str))
(define (b-goto line-number) line-number)

(define-exn end-program-signal exn:fail)
(define (b-end) (raise-end-program-signal))

(provide b-line b-statement b-rem b-print b-goto b-end)

(define-exn-srcloc line-not-found exn:fail)

(define (run line-table)
  (define line-vec (list->vector (sort (hash-keys line-table) <)))
  (with-handlers ([end-program-signal? void])
    (for/fold ([line-idx 0])
              ([i (in-naturals)])
      (unless (< line-idx (vector-length line-vec)) (b-end))
      (define line-num (vector-ref line-vec line-idx))
      (define line-proc (hash-ref line-table line-num))
      (define line-result (line-proc))
      (if (exact-positive-integer? line-result)
          (or (vector-member line-result line-vec)
              (raise-line-not-found (line-proc #:srcloc? #t)))
          (add1 line-idx)))))

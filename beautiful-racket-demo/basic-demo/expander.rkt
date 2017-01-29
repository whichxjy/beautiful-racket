#lang br/quicklang
(provide (rename-out [b-module-begin #%module-begin])
         (matching-identifiers-out #rx"^b-" (all-defined-out)))

(define-macro (b-module-begin (b-program LINE ...))
  (with-pattern
      ([(LINE-NUM ...)
        (filter-stx-prop 'b-line-number
                         (stx-flatten #'(LINE ...)))]
       [(LINE-ID ...) (prefix-ids "line-" #'(LINE-NUM ...))])
    #'(#%module-begin
       LINE ...
       (define line-table
         (apply hasheqv (append (list LINE-NUM LINE-ID) ...)))
       (run line-table))))

(define-macro (b-line LINE-NUMBER STATEMENT ...)
  (with-pattern
      ([LINE-NUMBER-ID (prefix-id "line-" #'LINE-NUMBER
                                  #:source #'LINE-NUMBER)]
       [ORIG-LOC caller-stx])
    (syntax/loc caller-stx
      (define (LINE-NUMBER-ID #:srcloc? [srcloc #f])
        (if srcloc
            (syntax-srcloc #'ORIG-LOC)
            (begin STATEMENT ...))))))

(define b-rem void)
(define (b-print [val ""]) (displayln val))
(define (b-sum . nums) (apply + nums))
(define (b-expr expr)
  (if (integer? expr) (inexact->exact expr) expr))

(struct $program-end-signal ())
(define (b-end) (raise ($program-end-signal)))

(struct $change-line-signal (val))
(define (b-goto expr) (raise ($change-line-signal expr)))

(define-exn line-not-found exn:fail)

(define (run line-table)
  (define line-vec
    (list->vector (sort (hash-keys line-table) <)))
  (with-handlers ([$program-end-signal? void])
    (for/fold ([line-idx 0])
              ([i (in-naturals)])
      (unless (< line-idx (vector-length line-vec)) (b-end))
      (define line-num (vector-ref line-vec line-idx))
      (define line-proc (hash-ref line-table line-num))
      (with-handlers
          ([$change-line-signal?
            (λ (cls)
              (define clsv ($change-line-signal-val cls))
              (or
               (and (exact-positive-integer? clsv)
                    (vector-member clsv line-vec))
               (raise-line-not-found
                (line-proc #:srcloc? #t))))])
        (line-proc)
        (add1 line-idx)))))

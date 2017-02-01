#lang br/quicklang
(provide (matching-identifiers-out #rx"^b-" (all-defined-out)))

(define-macro (b-line NUM STATEMENT ...)
  (with-pattern ([LINE-NUM (prefix-id "line-" #'NUM
                                      #:source #'NUM)])
    (syntax/loc caller-stx
      (define (LINE-NUM) (void) STATEMENT ...))))

(define-macro (b-module-begin (b-program LINE ...))
  (with-pattern
      ([((NAME NUM STMT ...) ...) #'(LINE ...)]
       [(LINE-FUNC ...) (prefix-id "line-" #'(NUM ...))])
    #'(#%module-begin
       LINE ...
       (define line-table
         (apply hasheqv (append (list NUM LINE-FUNC) ...)))
       (run line-table))))
(provide (rename-out [b-module-begin #%module-begin]))

(struct $program-end-signal ())
(define (b-end) (raise ($program-end-signal)))

(struct $change-line-signal (val))
(define (b-goto expr) (raise ($change-line-signal expr)))

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
               (error (format "error in line ~a: line ~a not found"
                              line-num clsv))))])
        (line-proc)
        (add1 line-idx)))))

(define b-rem void)
(define (b-print [val ""]) (displayln val))
(define (b-sum . nums) (apply + nums))
(define (b-num-expr expr)
  (if (integer? expr) (inexact->exact expr) expr))
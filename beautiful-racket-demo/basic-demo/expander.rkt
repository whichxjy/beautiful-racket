#lang br/quicklang

(define-exn-srcloc duplicate-line-number exn:fail)

(define-macro (b-module-begin (b-program LINE ...))
  #'(#%module-begin
     (define lines (sort (list LINE ...) #:key $line-number <))
     (unless (apply < (map $line-number lines))
       (raise-duplicate-line-number
        ($line-srcloc (check-duplicates lines = #:key $line-number))))
     (run lines)))
(provide (rename-out [b-module-begin #%module-begin]))

(struct $line (number thunk srcloc) #:transparent)

(define-macro (b-line LINE-NUMBER STATEMENT)
  (with-pattern ([CALLER-STX caller-stx])
    #'($line LINE-NUMBER (λ () STATEMENT) (syntax-srcloc #'CALLER-STX))))

(define (b-statement stmt) stmt)
(define (b-rem str) #f)
(define (b-print str) (displayln str))
(define (b-goto line-number) line-number)

(define-exn end-program-signal exn:fail)
(define (b-end) (raise-end-program-signal))

(provide b-line b-statement b-rem b-print b-goto b-end)

(define-exn-srcloc line-not-found exn:fail)

(define (run lines)
  (define line-vec (list->vector lines))
  (define line-idx-table (for/hasheqv ([(line idx) (in-indexed line-vec)])
                           (values ($line-number line) idx)))
  (with-handlers ([end-program-signal? void])
    (for/fold ([line-idx 0])
              ([i (in-naturals)])
      (unless (< line-idx (vector-length line-vec)) (b-end))
      (define this-line (vector-ref line-vec line-idx))
      (define this-thunk ($line-thunk this-line))
      (define this-result (this-thunk))
      (if (exact-positive-integer? this-result)
          (hash-ref line-idx-table this-result
                    (λ () (raise-line-not-found ($line-srcloc this-line))))
          (add1 line-idx)))))
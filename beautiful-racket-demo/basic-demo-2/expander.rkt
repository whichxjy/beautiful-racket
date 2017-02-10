#lang br/quicklang
(require "struct.rkt"
         "elements.rkt")
(provide (rename-out [b-module-begin #%module-begin])
         (all-from-out "elements.rkt"))

(define-macro (b-module-begin (b-program LINE ...))
  (with-pattern
      ([((b-line NUM STMT ...) ...) #'(LINE ...)]
       [(LINE-FUNC ...) (prefix-id "line-" #'(NUM ...))]
       [(VAR-NAME ...) (find-unique-var-names #'(LINE ...))])
    #'(#%module-begin
       (define VAR-NAME 0) ...
       LINE ...
       (define line-table
         (apply hasheqv (append (list NUM LINE-FUNC) ...)))
       (void (run line-table)))))

(begin-for-syntax
  (require racket/list)
  (define (find-unique-var-names stx)
    (remove-duplicates
     (for/list ([var-stx (in-list (syntax-flatten stx))]
                #:when (syntax-property var-stx 'b-id))
       var-stx)
     #:key syntax->datum)))

(define (run line-table)
  (define line-vec
    (list->vector (sort (hash-keys line-table) <)))
  (with-handlers ([end-program-signal? (λ (exn-val) (void))])
    (for/fold ([line-idx 0])
              ([i (in-naturals)]
               #:break (>= line-idx (vector-length line-vec)))
      (define line-num (vector-ref line-vec line-idx))
      (define line-func (hash-ref line-table line-num))
      (with-handlers
          ([change-line-signal?
            (λ (cls)
              (define clsv (change-line-signal-val cls))
              (or
               (and (exact-positive-integer? clsv)
                    (vector-member clsv line-vec))
               (line-func #:error (format "line ~a not found" clsv))))])
        (line-func)
        (add1 line-idx)))))



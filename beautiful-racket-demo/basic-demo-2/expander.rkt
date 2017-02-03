#lang br/quicklang
(require (for-syntax racket/list sugar/debug))
(provide (matching-identifiers-out #rx"^b-" (all-defined-out)))

(struct line-error (msg))

(define (handle-line-error num le)
  (error (format "error in line ~a: ~a" num (line-error-msg le))))

(define return-ks empty)

(define (b-gosub num-expr)
  (let/cc return-k
    (push! return-ks return-k)
    (b-goto num-expr)))

(define (b-return)
  (unless (pair? return-ks)
    (raise (line-error "return without gosub")))
  (define top-return-k (pop! return-ks))
  (top-return-k))

(define-macro (b-line NUM STATEMENT ...)
  (with-pattern ([LINE-NUM (prefix-id "line-" #'NUM
                                      #:source #'NUM)])
    (syntax/loc caller-stx
      (define (LINE-NUM)
        (with-handlers ([line-error? (λ (le) (handle-line-error NUM le))])
          (void) STATEMENT ...)))))

(define-for-syntax (find-unique-var-names stx)
  (remove-duplicates
   (for/list ([var-stx (in-list (syntax-flatten stx))]
              #:when (syntax-property var-stx 'b-id))
             var-stx)
   #:key syntax->datum))

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
(provide (rename-out [b-module-begin #%module-begin]))

(define-macro (b-let ID VAL)
  #'(set! ID VAL))

(struct end-program-signal ())
(struct change-line-signal (val))

(define (b-end) (raise (end-program-signal)))
(define (b-goto num-expr) (raise (change-line-signal num-expr)))

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
               (error (format "error in line ~a: line ~a not found"
                              line-num clsv))))])
        (line-func)
        (add1 line-idx)))))

(define (b-rem val) (void))
(define (b-print [val ""]) (displayln val))
(define (b-sum . nums) (apply + nums))
(define (b-num-expr expr)
  (if (integer? expr) (inexact->exact expr) expr))
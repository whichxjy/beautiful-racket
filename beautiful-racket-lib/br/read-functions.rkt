#lang racket/base
(require (for-syntax racket/base) syntax/strip-context)
(provide define-read-functions)

;; `define-read-functions` simplifies support for the standard reading API,
;; which asks for `read` and `read-syntax`.
;; in general, `read` is just the datum from the result of `read-syntax`.

(define-syntax (define-read-functions use-site-stx)
  (syntax-case use-site-stx ()
    [(_ (PATH PORT) BODY ...)
     (with-syntax ([READ (datum->syntax use-site-stx 'read)]
                   [READ-SYNTAX (datum->syntax use-site-stx 'read-syntax)])
       #'(begin           
           (provide READ READ-SYNTAX)
           (define (use-site-read-function PATH PORT)
             BODY ...) ; don't care whether this produces datum or syntax
           
           (define (READ-SYNTAX path port)
             ;; because `read-syntax` must produce syntax
             ;; coerce a datum result to syntax if needed (à la `with-syntax`)
             (define result-syntax (let ([output (use-site-read-function path port)])
                                     (if (syntax? output)
                                         output 
                                         (datum->syntax #f output))))
             ;; because `read-syntax` must produce syntax without context
             ;; see http://docs.racket-lang.org/guide/hash-lang_reader.html
             ;; "a `read-syntax` function should return a syntax object with no lexical context"
             (strip-context result-syntax)) 
           
           (define (READ port)
             ; because `read` must produce a datum
             (let ([output (use-site-read-function #f port)])
               (if (syntax? output)
                   (syntax->datum output)
                   output)))))]))

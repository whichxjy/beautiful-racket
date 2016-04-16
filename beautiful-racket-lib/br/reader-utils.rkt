#lang racket/base
(require (for-syntax racket/base racket/syntax) syntax/strip-context)
(provide define-read-and-read-syntax)

;; `define-read-functions` simplifies support for the standard reading API,
;; which asks for `read` and `read-syntax`.
;; in general, `read` is just the datum from the result of `read-syntax`.

(define-syntax (define-read-and-read-syntax calling-site-stx)
  (syntax-case calling-site-stx ()
    [(_ (PATH PORT) BODY ...)
     (let ([internal-prefix (gensym)])
       (with-syntax ([READ (datum->syntax calling-site-stx 'read)]
                     [READ-SYNTAX (datum->syntax calling-site-stx 'read-syntax)]
                     ;; use prefixed names to prevent namespace collisions with possibly existing `read` & `read-syntax`
                     [INTERNAL-READ (format-id #'here "~a-~a" internal-prefix 'read)]
                     [INTERNAL-READ-SYNTAX (format-id #'here "~a-~a" internal-prefix 'read-syntax)])
         #'(begin           
             (provide (rename-out [INTERNAL-READ READ]
                                  [INTERNAL-READ-SYNTAX READ-SYNTAX]))
             (define (calling-site-function PATH PORT)
               BODY ...) ; don't care whether this produces datum or syntax
             
             (define INTERNAL-READ-SYNTAX
               (procedure-rename (λ (path port) ; rename proc so it looks right in the REPL (otherwise retains internal prefix name)
                                   ;; because `read-syntax` must produce syntax
                                   ;; coerce a datum result to syntax if needed (à la `with-syntax`)
                                   (define result-syntax (let ([output (calling-site-function path port)])
                                                           (if (syntax? output)
                                                               output 
                                                               (datum->syntax #f output))))
                                   ;; because `read-syntax` must produce syntax without context
                                   ;; see http://docs.racket-lang.org/guide/hash-lang_reader.html
                                   ;; "a `read-syntax` function should return a syntax object with no lexical context"
                                   (strip-context result-syntax)) 'READ-SYNTAX)) 
             
             (define INTERNAL-READ
               (procedure-rename (λ (port)
                                   ; because `read` must produce a datum
                                   (let ([output (calling-site-function #f port)])
                                     (if (syntax? output)
                                         (syntax->datum output)
                                         output))) 'READ)))))]))
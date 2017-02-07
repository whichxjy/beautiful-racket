#lang br
(require "structs.rkt")
(provide (all-defined-out))

(define-macro (b-line NUM STATEMENT ...)
  (with-pattern ([LINE-NUM (prefix-id "line-" #'NUM
                                      #:source #'NUM)])
    (syntax/loc caller-stx
      (define (LINE-NUM)
        (with-handlers ([line-error? (λ (le) (handle-line-error NUM le))])
          (void) STATEMENT ...)))))

(define (handle-line-error num le)
  (error (format "error in line ~a: ~a" num (line-error-msg le))))

(define (raise-line-error line-error-or-str)
  (raise (if (string? line-error-or-str)
             (line-error line-error-or-str)
             line-error-or-str)))
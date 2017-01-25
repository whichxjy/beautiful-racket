#lang racket/base
(require (for-syntax racket/base br/syntax) br/define racket/match)
(provide (all-defined-out))

(define-macro (define-exn EXN-ID BASE-EXN)
  (with-pattern ([RAISE-EXN-ID (prefix-id "raise-" #'EXN-ID)])
    #'(begin
        (struct EXN-ID BASE-EXN () #:transparent)
        (define (RAISE-EXN-ID)
          (raise (EXN-ID (format "error: ~a" 'EXN-ID) (current-continuation-marks)))))))

(define-macro (define-exn-srcloc EXN-ID BASE-EXN)
  (with-pattern ([RAISE-EXN-ID (prefix-id "raise-" #'EXN-ID)])
    #'(begin
        (define-struct (EXN-ID BASE-EXN)
          (a-srcloc) #:transparent
          #:property prop:exn:srclocs
          (lambda (a-struct)
            (match a-struct
              [(struct EXN-ID
                 (msg marks a-srcloc))
               (list a-srcloc)])))
        (define RAISE-EXN-ID
          (case-lambda
            [(srcloc)
             (raise (EXN-ID (format "error: ~a" 'EXN-ID) (current-continuation-marks) srcloc))]
            [()
             (raise (EXN-ID (format "error: ~a" 'EXN-ID) (current-continuation-marks)))])))))
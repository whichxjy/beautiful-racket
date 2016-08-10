#lang racket/base
(require (for-syntax racket/base racket/syntax))
(provide (all-defined-out))

(define-syntax (report stx)
  (syntax-case stx ()
    [(_ EXPR) #'(report EXPR EXPR)]
    [(_ EXPR NAME)
     #'(let ([expr-result EXPR]) 
         (eprintf "~a = ~v\n" 'NAME expr-result)
         expr-result)]))

(define-syntax (report-datum stx)
  (syntax-case stx ()
    [(_ STX-EXPR)
     (with-syntax ([datum (syntax->datum #'STX-EXPR)])
       #'(report-datum STX-EXPR datum))]
    [(_ STX-EXPR NAME)
     #'(let () 
         (eprintf "~a = ~v\n" 'NAME (syntax->datum STX-EXPR))
         STX-EXPR)]))

(define-syntax-rule (define-multi-version MULTI-NAME NAME)
  (define-syntax-rule (MULTI-NAME x (... ...))
    (begin (NAME x) (... ...))))

(define-multi-version report* report)
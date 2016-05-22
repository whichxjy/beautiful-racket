#lang racket/base
(require (for-syntax racket/base racket/syntax))
(provide (all-defined-out))

(define-syntax (report stx)
  (syntax-case stx ()
    [(_ expr) #'(report expr expr)]
    [(_ expr name)
     #'(let ([expr-result expr]) 
         (eprintf "~a = ~v\n" 'name expr-result)
         expr-result)]))

(define-syntax (report-datum stx)
  (syntax-case stx ()
    [(_ stx-expr) (with-syntax ([datum (syntax->datum #'stx-expr)])
                    #'(report-datum stx-expr datum))]
    [(_ stx-expr name)
     #'(let () 
         (eprintf "~a = ~v\n" 'name (syntax->datum stx-expr))
         stx-expr)]))

(define-syntax-rule (define-multi-version multi-name name)
  (define-syntax-rule (multi-name x (... ...))
    (begin (name x) (... ...))))

(define-multi-version report* report)
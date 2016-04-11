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

(define-syntax-rule (define-multi-version multi-name name)
  (define-syntax-rule (multi-name x (... ...))
    (begin (name x) (... ...))))

(define-multi-version report* report)
#lang racket/base
(require (for-syntax racket/base racket/syntax "helper.rkt" racket/list))
(provide (all-defined-out))


(define-syntax (handle-require stx)
  (syntax-case stx ()
    [(_ prefix [suffix arg] ...)
     (with-syntax ([(prefix-suffix ...) (map (λ(s) (format-id s "~a-~a" #'prefix s)) (syntax->list #'(suffix ...)))]
                   [module-name (format "~a.hdl.rkt" (syntax->datum #'prefix))])
       #'(begin
           (local-require module-name (for-syntax module-name))
           (handle-wires [prefix-suffix arg] ...)))]))


(define-syntax (handle-part stx)
  (syntax-case stx ()
    [(_ prefix [suffix arg] ...)
     (with-syntax ([(prefix-suffix ...) (map (λ(s) (format-id s "~a-~a" #'prefix s)) (syntax->list #'(suffix ...)))]
                   [module-name (format "~a.hdl.rkt" (syntax->datum #'prefix))])
       #'(begin
           (require module-name (for-syntax module-name))
           (handle-wires [prefix-suffix arg] ...)))]))


(define-syntax (handle-wires stx)
  (syntax-case stx ()
    [(_ [wire arg] ...)
     (let ()
       (define-values (in-wires out-wires) (partition (λ(stx) (let ([wire (car (syntax->list stx))])
                                                                (input-wire? (syntax-local-eval wire)))) (syntax->list #'([wire arg] ...))))
       (with-syntax ([([in-wire in-arg] ...) in-wires]
                     [([out-wire out-arg] ...) out-wires])
         #'(begin
             (define out-arg (λ () (in-wire (in-arg)) ... (out-wire))) ...)))]))
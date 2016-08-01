#lang br/quicklang
(provide read-syntax
         (rename-out [stacker-module-begin #%module-begin])
         + *)

(define (read-syntax src-path in-port)
  (define args (port->list read in-port))
  (define module-datum `(module funstacker-mod br/demo/funstacker
                          ,@args))
  (datum->syntax #f module-datum))

(define-macro (stacker-module-begin ARG ...)
  #'(#%module-begin
     (define stack-result
       (for/fold ([stack empty])
                 ([arg (in-list (list ARG ...))])
         (push arg stack)))
     (display (first stack-result))))

(define (push arg stack)
  (if (number? arg)
      (cons arg stack)
      (cons (arg (first stack) (second stack)) (drop stack 2))))

(module+ test 
  (require rackunit)
  (check-equal? (with-output-to-string (λ () (dynamic-require "funstacker-test.rkt" #f))) "36"))
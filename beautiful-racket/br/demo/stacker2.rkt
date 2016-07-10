#lang br/quicklang
#:read-syntax stacker-read-syntax
#:#%module-begin stacker-module-begin

(define (stacker-read-syntax src-path in-port)
  (define stack-args (port->list read in-port))
  (strip-context
   (with-pattern ([(STACK-ARG ...) stack-args])
     #'(module stacker2-mod br/demo/stacker2
         STACK-ARG ...))))

(define-macro (stacker-module-begin STACK-ARG ...)
  #'(#%module-begin
     (define stack-result
       (for/fold ([stack null])
                 ([arg (in-list (list STACK-ARG ...))])
         (push arg stack)))
     (display (first stack-result))))

(define (push arg stack)
  (cond
    [(number? arg) (cons arg stack)]
    [else
     (define result (arg (first stack) (second stack)))
     (cons result (drop stack 2))]))

(provide + *)

(module+ test 
  (require rackunit)
  (check-equal? (with-output-to-string (λ () (dynamic-require "stacker2-test.rkt" #f))) "36"))
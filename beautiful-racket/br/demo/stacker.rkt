#lang br/quicklang
#:read-syntax stacker-read-syntax
#:#%module-begin stacker-module-begin

(define (stacker-read-syntax src-path in-port)
  (define stack-args (port->list read in-port))
  (strip-context
   (with-pattern ([(STACK-ARG ...) stack-args])
     #'(module stacker-mod br/demo/stacker
         (push STACK-ARG) ...))))

(define-macro (stacker-module-begin PUSH-STACK-ARG ...)
  #'(#%module-begin
     PUSH-STACK-ARG ...
     (display (first stack))))

(define stack empty)

(define (push arg)
  (cond
    [(number? arg) (set! stack (cons arg stack))]
    [else
     (define result (arg (first stack) (second stack)))
     (set! stack (cons result (drop stack 2)))]))
(provide push)

(provide + *)

(module+ test 
  (require rackunit)
  (check-equal? (with-output-to-string (λ () (dynamic-require "stacker-test.rkt" #f))) "36"))
#lang br/quicklang

(define (read-syntax src-path in-port)
  (define stack-args (port->list read in-port))
  (strip-context (with-pattern ([(STACK-ARG ...) stack-args])
                   #'(module stacker-mod br/demo/stacker
                       (push STACK-ARG) ...))))
(provide read-syntax)

(define-macro (stacker-module-begin PUSH-STACK-ARG ...)
  #'(#%module-begin
     PUSH-STACK-ARG ...
     (display (first stack))))
(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)

(define (push arg)
  (cond
    [(number? arg) (set! stack (cons arg stack))]
    [else
     (define result (arg (first stack) (second stack)))
     (set! stack (cons result (drop stack 2)))]))
(provide push)

(provide + *)

#;(module+ test 
  (require rackunit)
  (check-equal? (with-output-to-string (λ () (dynamic-require "stacker-test.rkt" #f))) "36"))
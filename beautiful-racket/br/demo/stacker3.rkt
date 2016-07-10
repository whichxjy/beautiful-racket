#lang br/quicklang
#:read-syntax stacker-read-syntax
#:#%module-begin stacker-module-begin

(define (stacker-read-syntax src-path in-port)
  (strip-context
   #`(module stacker3-mod br/demo/stacker3
       #,@(port->list read in-port))))

(define-macro (stacker-module-begin STACK-ARG ...)
  #'(#%module-begin
     (display
      (first
       (foldl (λ(arg stack)
                (if (number? arg)
                    (cons arg stack)
                    (cons (arg (car stack) (cadr stack)) (cddr stack))))
              null (list STACK-ARG ...))))))

(provide + *)

(module+ test 
  (require rackunit)
  (check-equal? (with-output-to-string (λ () (dynamic-require "stacker2-test.rkt" #f))) "36"))
#lang br/quicklang


(define (read-syntax src-path in-port)
  (define stack-args (port->list read in-port))
  (strip-context
   (with-pattern ([(STACK-ARG ...) stack-args])
     #'(module stacker2-mod br/demo/stacker2
         STACK-ARG ...))))
(provide read-syntax)

(define-macro (stacker-module-begin STACK-ARG ...)
  #'(#%module-begin
     (define stack-result
       (for/fold ([stack null])
                 ([arg (in-list (list STACK-ARG ...))])
         (push arg stack)))
     (display (first stack-result))))
(provide (rename-out [stacker-module-begin #%module-begin]))

(define (push arg stack)
  (if (number? arg)
      (cons arg stack)
      (let* ([op arg]
             [result (op (first stack) (second stack))])
        (cons result (drop stack 2)))))

(provide + * #%app #%datum #%top-interaction)

(module+ test 
  (require rackunit)
  (check-equal? (with-output-to-string (λ () (dynamic-require "stacker2-test.rkt" #f))) "36"))
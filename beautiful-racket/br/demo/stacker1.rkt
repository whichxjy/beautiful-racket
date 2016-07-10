#lang br/quicklang

(define (read-syntax src-path in-port)
  (define args (port->list read in-port))
  (define module-datum `(module stacker-mod br/demo/stacker1
                          ,@args))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (stacker-module-begin ARG ...)
  #'(#%module-begin
     (push ARG) ...
     (display (first stack))))
(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)

(define (pop-stack!)
  (define item (first stack))
  (set! stack (rest stack))
  item)

(define (push-stack! item) (set! stack (cons item stack)))

(define (push arg)
  (cond
    [(number? arg) (push-stack! arg)]
    [else
     (define op-result (arg (pop-stack!) (pop-stack!))) 
     (push-stack! op-result)]))

(provide + *)

(module+ test 
  (require rackunit)
  (check-equal? (with-output-to-string (λ () (dynamic-require "stacker1-test.rkt" #f))) "36"))
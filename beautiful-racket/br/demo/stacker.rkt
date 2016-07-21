#lang br/quicklang

(define (read-syntax path port)
  (define args (port->lines port))
  (define arg-datums (format-datums '(handle ~a) args))
  (define module-datum `(module stacker-mod br/demo/stacker
                          ,@arg-datums))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (stacker-module-begin ARG-DATUM-STX ...)
  #'(#%module-begin
     ARG-DATUM-STX ...
     (display (pop-stack!))))
(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)

(define (pop-stack!)
  (define item (first stack))
  (set! stack (rest stack))
  item)

(define (push-stack! item)
  (set! stack (cons item stack)))

(define (handle [arg #f])
  (cond
    [(number? arg) (push-stack! arg)]
    [(procedure? arg)
     (define op-result (arg (pop-stack!) (pop-stack!))) 
     (push-stack! op-result)]))

(provide handle + *)

(module+ test 
  (require rackunit)
  (check-equal? (with-output-to-string (λ () (dynamic-require "stacker-test.rkt" #f))) "36"))
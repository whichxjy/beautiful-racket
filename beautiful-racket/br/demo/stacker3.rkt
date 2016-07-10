#lang br/quicklang
(provide read-syntax
         (rename-out [stacker-module-begin #%module-begin])
         + *)

(define (read-syntax path port)
  (datum->syntax #f
                 `(module stacker3-mod br/demo/stacker3
                      ,@(port->list read port))))

(define-macro (stacker-module-begin ARG ...)
  #'(#%module-begin
     (display (first
               (foldl
                (λ (x xs)
                  (if (number? x)
                      (cons x xs)
                      (cons (x (car xs) (cadr xs)) (cddr xs))))
                null (list ARG ...))))))

(module+ test 
  (require rackunit)
  (check-equal? (with-output-to-string (λ () (dynamic-require "stacker3-test.rkt" #f))) "36"))
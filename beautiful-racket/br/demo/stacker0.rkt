#lang br/quicklang

(define (read-syntax src-path in-port)
  (define args (port->lines in-port))
  (define module-datum `(module stacker-mod br/demo/stacker0
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
    [(equal? arg "+")
     (define sum (+ (pop-stack!) (pop-stack!)))
     (push-stack! sum)]
    [(equal? arg "*")
     (define product (* (pop-stack!) (pop-stack!)))
     (push-stack! product)]
    [(string->number arg) (push-stack! (string->number arg))]))


(module+ test 
  (require rackunit)
  (check-equal? (with-output-to-string (λ () (dynamic-require "stacker0-test.rkt" #f))) "36"))
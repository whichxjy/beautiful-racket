#lang br

(define (read-syntax src-path in-port)
  (define lines (remove-blank-lines (port->lines in-port)))
  (define (make-exec-datum line) (format-datum '(exec ~a) line))
  (define exec-exprs (map make-exec-datum lines))
  (strip-context (with-pattern ([(EXEC-EXPR ...) exec-exprs])
                   #'(module stacker-mod br/demo/stacker
                       EXEC-EXPR ...))))
(provide read-syntax)

(define-macro (stacker-module-begin SRC-LINE ...)
  #'(#%module-begin
     SRC-LINE ...
     (display (first stack))))
(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)
(define (push num) (set! stack (cons num stack)))
(provide push)

(define-cases exec
  [(_ func num) (func num)]
  [(_ op) (define result (op (first stack) (second stack)))
          (set! stack (cons result (drop stack 2)))])
(provide exec)

(provide + * #%app #%datum #%top-interaction)

(module+ test 
  (require rackunit)
  (check-equal? (with-output-to-string (λ () (dynamic-require "stacker-test.rkt" #f))) "36"))
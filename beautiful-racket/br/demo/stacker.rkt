#lang br

(define (read-syntax source-path input-port)
  (define src-strs (remove-blank-lines (port->lines input-port)))
  (define (make-datum str) (format-datum '(dispatch ~a) str))
  (define src-exprs (map make-datum src-strs))
  (strip-context
   (with-pattern ([(SRC-EXPR ...) (map make-datum src-strs)])
                 #'(module stacker-mod br/demo/stacker
                     SRC-EXPR ...))))
(provide read-syntax)

(define-macro (stacker-module-begin READER-LINE ...)
  #'(#%module-begin
     READER-LINE ...
     (display (first stack))))
(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)
(define (push num) (set! stack (cons num stack)))
(provide push)

(define-cases dispatch
  [(_ push num) (push num)]
  [(_ op) (define op-result (op (first stack) (second stack)))
          (set! stack (cons op-result (drop stack 2)))])
(provide dispatch)

(provide + * #%app #%datum #%top-interaction)
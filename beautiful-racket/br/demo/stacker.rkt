#lang br
(define (read-syntax source-path input-port)
  (define src-strs (remove-blank-lines (port->lines input-port)))
  (define (make-datum str) (format-datum '(dispatch ~a) str))
  (define src-exprs (map make-datum src-strs))
  (strip-context
   (inject-syntax ([#'(SRC-EXPR ...) src-exprs])
                  #'(module stacker-mod br/demo/stacker
                      SRC-EXPR ...))))
(provide read-syntax)

(define #'(stacker-module-begin _READER-LINE ...)
  #'(#%module-begin
     _READER-LINE ...
     (display (first stack))))
(provide (rename-out [stacker-module-begin #%module-begin]))
(provide #%top-interaction)

(define stack empty)
(define (push num) (set! stack (cons num stack)))
(provide push)

(define (dispatch arg-1 [arg-2 #f])
  (cond
    [(number? arg-2) (push arg-2)]
    [else
     (define op arg-1)
     (define op-result (op (first stack) (second stack)))
     (set! stack (cons op-result (drop stack 2)))]))
(provide dispatch)

(provide + *)
(provide #%app #%datum)
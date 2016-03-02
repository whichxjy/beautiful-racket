#lang br

(provide #%top-interaction) ; activates the REPL

;; reader goes first and
;; 1) converts surface syntax into Rackety expressions (wrapping in `(inst ...)`)
;; 2) creates a module specifying semantics source (in this case, same module)
(provide read-syntax)
(define (read-syntax src-path src-input-port)
  (define inst-datums (for/list ([src-line (in-lines src-input-port)]
                                #:when (regexp-match #px"\\w" src-line))
                              (format-datum '(inst ~a) src-line)))
  (syntax-let ([#'(inst-stx ...) inst-datums]) ; ok to bind with non-syntax
              #'(module stack-lang-module "stack.rkt" 
                  inst-stx ...)))

(define stack empty)

;; semantics always start with #%module-begin, which unwraps the content of the module and rewraps it
(provide (rename-out [stack-module-begin #%module-begin]))
(define #'(stack-module-begin instructions ...)
  #'(#%module-begin
     instructions ...
     (first stack)))


;; then file is processed like a normal Racket file.

(provide inst)
(define (inst . args)
  (if (<= 1 (length args) 2)
      (let ([proc (first args)])
        (apply proc (cdr args)))
      (void)))


(provide push)
(define (push arg)
  (display "push: ")
  (cond
    [(procedure? arg)
     (displayln (format "got ~a, replacing ~a and ~a with result"  arg (first stack) (second stack) ))
     (set! stack (cons (arg (first stack) (second stack)) (cddr stack)))]
    [else (displayln (format "storing value ~a" arg))
          (set! stack (cons arg stack))])
  (displayln stack))

;; exercises

(provide pop)
(define (pop)
  (display "pop: ")
  (displayln (format "got ~a" (car stack)))
  (set! stack (cdr stack))
  (displayln stack))

(provide swap)
(define (swap)
  (display "swap: ")
  (displayln (format "~a and ~a" (first stack) (second stack)))
  (set! stack (list* (second stack) (first stack) (cddr stack)))
  (displayln stack))
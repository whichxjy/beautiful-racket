#lang br

(provide #%top-interaction) ; activates the REPL

;; reader goes first and
;; 1) converts surface syntax into Rackety expressions (wrapping in `(inst ...)`)
;; 2) creates a module specifying semantics source (in this case, same module)
(provide read-syntax)
(define (read-syntax src-path src-input-port)
  (define expr-datums (for/list ([src-line (in-lines src-input-port)]
                                 #:when (regexp-match #px"\\w" src-line))
                                (format-datum '~a src-line)))
  (syntax-let ([#'(expr-stx ...) expr-datums]) ; ok to bind with non-syntax
              #'(module stack-compiler-module "stack-compiler.rkt" 
                  expr-stx ...)))


;; semantics always start with #%module-begin, which unwraps the content of the module and rewraps it
(provide (rename-out [stack-module-begin #%module-begin]))
(define #'(stack-module-begin expr ...)
  #'(#%module-begin
     (stackify (quote expr)) ...))


(define (stackify quoted-expr)
  (define pushes
    (let loop ([x quoted-expr])
      (cond
        [(list? x)
         (match-define (list op args ...) x)
         (append (make-list (sub1 (length args)) op) (flatten (map loop args)))]
        [else x])))
  (for-each displayln (map (λ(p) (format "push ~a" p)) (reverse pushes))))
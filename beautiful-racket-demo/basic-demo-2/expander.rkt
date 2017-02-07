#lang br/quicklang
(require "runtime.rkt"
         "run.rkt"
         "line.rkt"
         "expr.rkt"
         "misc.rkt"
         "flow.rkt")
(provide (rename-out [b-module-begin #%module-begin])
         (all-from-out "line.rkt"
                       "expr.rkt"
                       "misc.rkt"
                       "flow.rkt"))

(define-macro (b-module-begin (b-program LINE ...))
  (with-pattern
      ([((b-line NUM STMT ...) ...) #'(LINE ...)]
       [(LINE-FUNC ...) (prefix-id "line-" #'(NUM ...))]
       [(VAR-NAME ...) (find-unique-var-names #'(LINE ...))])
    #'(#%module-begin
       (module configure-runtime br
         (require basic-demo-2/runtime)
         (current-basic-port (current-output-port)))
       (define VAR-NAME 0) ...
       (provide VAR-NAME ...)
       LINE ...
       (define line-table
         (apply hasheqv (append (list NUM LINE-FUNC) ...)))
       (void (parameterize ([current-output-port
                             (or (current-basic-port) (open-output-nowhere))])
               (run line-table))))))

(begin-for-syntax
  (require racket/list)
  (define (find-unique-var-names stx)
    (remove-duplicates
     (for/list ([var-stx (in-list (syntax-flatten stx))]
                #:when (syntax-property var-stx 'b-id))
               var-stx)
     #:key syntax->datum)))



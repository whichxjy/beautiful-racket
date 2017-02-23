#lang br/quicklang
(require "struct.rkt" "run.rkt" "elements.rkt" "runtime.rkt")
(provide (rename-out [b-module-begin #%module-begin])
         (all-from-out "elements.rkt"))

(define-macro (b-module-begin (b-program LINE ...))
  (with-pattern
      ([((b-line NUM STMT ...) ...) #'(LINE ...)]
       [(LINE-FUNC ...) (prefix-id "line-" #'(NUM ...))]
       [(VAR-ID ...) (find-property 'b-id #'(LINE ...))]
       [(REQ-SPEC ...) (find-property 'b-require-spec #'(LINE ...))])
    #'(#%module-begin
       (module configure-runtime br
         (require "runtime.rkt")
         (current-basic-port (current-output-port))
         (configure-repl!))
       (require REQ-SPEC) ...
       (define VAR-ID 0) ...
       (provide VAR-ID ...)
       LINE ...
       (define line-table
         (apply hasheqv (append (list NUM LINE-FUNC) ...)))
       (parameterize ([current-output-port
                             (or (current-basic-port) (open-output-nowhere))])
               (void (run line-table))))))

(begin-for-syntax
  (require racket/list)
  (define (find-property which line-stxs)
    (remove-duplicates
     (for/list ([stx (in-list (stx-flatten line-stxs))]
                #:when (syntax-property stx which))
       stx)
     #:key syntax->datum)))


#lang br/quicklang
(require "struct.rkt" "run.rkt" "elements.rkt" "setup.rkt")
(provide (rename-out [b-module-begin #%module-begin])
         (all-from-out "elements.rkt"))

(define-macro (b-module-begin (b-program LINE ...))
  (with-pattern
      ([((b-line NUM STMT ...) ...) #'(LINE ...)]
       [(LINE-FUNC ...) (prefix-id "line-" #'(NUM ...))]
       [(VAR-ID ...) (find-property 'b-id #'(LINE ...))]
       [(IMPORT-NAME ...)
        (find-property 'b-import-name #'(LINE ...))]
       [(EXPORT-NAME ...)
        (find-property 'b-export-name #'(LINE ...))]
       [((SHELL-ID SHELL-VAL) ...)
        (for/list ([(val idx) (in-indexed (current-command-line-arguments))])
          (list (suffix-id #'arg idx #:context caller-stx) val))])
    #'(#%module-begin
       (module configure-runtime br
         (require basic-demo-3/setup)
         (do-setup!))
       (require IMPORT-NAME) ...
       (provide EXPORT-NAME ...)
       (define VAR-ID 0) ...
       (set! SHELL-ID SHELL-VAL) ...
       LINE ...
       (define line-table
         (apply hasheqv (append (list NUM LINE-FUNC) ...)))
       (parameterize
           ([current-output-port (basic-output-port)])
         (void (run line-table))))))

(begin-for-syntax
  (require racket/list)
  (define (find-property which line-stxs)
    (remove-duplicates
     (for/list ([stx (in-list (stx-flatten line-stxs))]
                #:when (syntax-property stx which))
       stx)
     #:key syntax->datum)))


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
        (make-shell-ids-and-values caller-stx)] 
       [(UNIQUE-ID ...)
        (unique-ids
         (syntax->list #'(VAR-ID ... SHELL-ID ...)))])
    #'(#%module-begin
       (module configure-runtime br
         (require basic-demo-3/setup)
         (do-setup!))
       (require IMPORT-NAME) ...
       (provide EXPORT-NAME ...)
       (define UNIQUE-ID 0) ...
       (set! SHELL-ID SHELL-VAL) ...
       LINE ...
       (define line-table
         (apply hasheqv (append (list NUM LINE-FUNC) ...)))
       (parameterize
           ([current-output-port (basic-output-port)])
         (void (run line-table))))))

(begin-for-syntax
  (require racket/list)
  
  (define (unique-ids stxs)
    (remove-duplicates stxs #:key syntax->datum))

  (define (find-property which line-stxs)
    (unique-ids
     (for/list ([stx (in-list (stx-flatten line-stxs))]
                #:when (syntax-property stx which))
       stx)))

  (define (make-shell-ids-and-values ctxt)
    (for/list ([idx (in-naturals)]
               [val (current-command-line-arguments)])
      (with-pattern
          ([SHELL-ID (suffix-id #'arg idx #:context ctxt)]
           [SHELL-VALUE (or (string->number val) val)])
        #'(SHELL-ID SHELL-VALUE)))))


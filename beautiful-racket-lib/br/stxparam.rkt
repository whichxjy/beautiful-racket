#lang racket/base
(require (for-syntax racket/base) racket/stxparam racket/splicing)
(provide splicing-syntax-parameterize
         define-syntax-parameters
         define-language-variables
         inject-language-variables
         (rename-out [br:define-syntax-parameter define-syntax-parameter]))

(define-syntax (br:define-syntax-parameter stx)
  (syntax-case stx ()
    [(_ ID STX)
     #'(define-syntax-parameter ID STX)]
    [(_ ID)
     #'(define-syntax-parameter ID (λ (stx) 
                                     (raise-syntax-error (syntax-e stx) "parameter not set")))]))

(define-syntax-rule (define-syntax-parameters ID ...)
  (begin (br:define-syntax-parameter ID) ...))

(define-syntax define-language-variables (make-rename-transformer #'define-syntax-parameters))

(define-syntax (inject-language-variables stx)
  (syntax-case stx ()
    [(_ ([VAR-PARAM INITIAL-VALUE] ...) LANG-CODE ...)
     (with-syntax ([(INTERNAL-NAME ...) (generate-temporaries #'(VAR-PARAM ...))])
     #'(splicing-syntax-parameterize ;; need to use splicing version in a module-begin to compose with requires etc. that might be in lang code
           ([VAR-PARAM (make-rename-transformer #'INTERNAL-NAME)] ...)
         (define INTERNAL-NAME INITIAL-VALUE) ...
         (provide (rename-out [INTERNAL-NAME VAR-PARAM] ...))
         LANG-CODE ...))]))

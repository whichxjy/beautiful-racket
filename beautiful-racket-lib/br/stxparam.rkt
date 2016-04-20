#lang racket/base
(require (for-syntax racket/base) racket/stxparam racket/splicing)
(provide splicing-syntax-parameterize
         define-syntax-parameters
         define-language-variables
         define-language-variable
         inject-language-variables
         (rename-out [br:define-syntax-parameter define-syntax-parameter]))

(define-syntax (br:define-syntax-parameter stx)
  (syntax-case stx ()
    [(_ ID STX) #'(define-syntax-parameter ID STX)]
    [(_ [ID VAL]) #'(define-syntax-parameter ID (λ (stx) #'VAL))]
    [(_ ID) #'(define-syntax-parameter ID
                (λ (stx) (raise-syntax-error (syntax-e stx) "parameter not set")))]))

(define-syntax-rule (define-syntax-parameters ID ...)
  (begin (br:define-syntax-parameter ID) ...))

(define-syntax-rule (define-language-variable ID VAL)
  (br:define-syntax-parameter [ID VAL]))

(define-syntax-rule (define-language-variables [ID VAL] ...)
  (begin (define-language-variable ID VAL) ...))

(define-syntax (inject-language-variables stx)
  (syntax-case stx ()
    [(_ (VAR-PARAM ...) LANG-CODE ...)
     (with-syntax ([(HOLDS-ORIG-PARAM-VALUE ...) (generate-temporaries #'(VAR-PARAM ...))]
                   [(INTERNAL-NAME ...) (generate-temporaries #'(VAR-PARAM ...))])
       ;; need to use splicing expressions in a module-begin to compose with requires etc. that might be in lang code
       #'(splicing-let ([HOLDS-ORIG-PARAM-VALUE VAR-PARAM] ...)
           (splicing-syntax-parameterize 
               ([VAR-PARAM (make-rename-transformer #'INTERNAL-NAME)] ...)
             (define INTERNAL-NAME HOLDS-ORIG-PARAM-VALUE) ...
             (provide (rename-out [INTERNAL-NAME VAR-PARAM] ...))
             LANG-CODE ...)))]))

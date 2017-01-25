#lang racket/base
(require (for-syntax racket/base racket/syntax)
         racket/list
         racket/syntax
         syntax/strip-context
         br/define
         br/private/syntax-flatten)
(provide (all-defined-out)
         syntax-flatten
         (rename-out [strip-context strip-bindings]
                     [replace-context replace-bindings]))

(module+ test
  (require rackunit))


(define-macro (syntax-match STX-ARG [(syntax PATTERN) BODY ...] ...)
  #'(syntax-case STX-ARG ()
      [PATTERN BODY ...] ...))


(define-macro-cases with-pattern
  [(_ () . BODY) #'(begin . BODY)]
  [(_ ([SID SID-STX] STX ...) . BODY)
   #'(with-syntax ([SID SID-STX])
       (with-pattern (STX ...) . BODY))]
  [(_ ([SID] STX ...) . BODY) ; standalone id
   #'(with-pattern ([SID SID] STX ...) . BODY)]) ; convert to previous case


(define (check-syntax-list-argument caller-name arg)
  (cond
    [(and (syntax? arg) (syntax->list arg))]
    [(list? arg) arg]
    [else (raise-argument-error caller-name "list of syntax, or syntaxed list" arg)]))


(define-macro (define-listy-macro MACRO-ID LIST-FUNC)
  #'(define-macro (MACRO-ID STX-LIST LITERALS . MATCHERS)
      #'(LIST-FUNC
         (λ(stx-item)
           (with-handlers ([exn:fail:syntax? (λ (exn) #f)])
             (syntax-case stx-item LITERALS
               . MATCHERS)))
         (check-syntax-list-argument 'MACRO-ID STX-LIST))))

(define-listy-macro syntax-case-partition partition)
(define-listy-macro syntax-case-filter filter)
(define-listy-macro syntax-case-map map)


(define-macro (reformat-id FMT ID0 ID ...)
  #'(format-id ID0 FMT ID0 ID ...))


(define-macro (format-string FMT ID0 ID ...)
  #'(datum->syntax ID0 (format FMT (syntax->datum ID0) (syntax->datum ID) ...)))


(define-macro (->unsyntax X)
  #'(if (syntax? X)
        (syntax->datum X)
        X))


(define-macro (prefix-id PREFIX ... BASE-OR-BASES)
  #'(let* ([bobs BASE-OR-BASES]
           [got-single? (and (not (list? bobs)) (not (syntax->list bobs)))]
           [bases (if got-single?
                      (list bobs)
                      bobs)]
           [result (syntax-case-map
                    bases ()
                    [base (format-id #'base "~a~a"
                                     (string-append (format "~a" (->unsyntax PREFIX)) ...)
                                     (syntax-e #'base))])])
      (if got-single? (car result) result)))


(define-macro (infix-id PREFIX BASE-OR-BASES SUFFIX ...)
  #'(let* ([bobs BASE-OR-BASES]
           [got-single? (and (not (list? bobs)) (not (syntax->list bobs)))]
           [bases (if got-single?
                      (list bobs)
                      bobs)]
           [result (syntax-case-map
                    bases ()
                    [base (format-id #'base "~a~a~a"
                                     (->unsyntax PREFIX)
                                     (syntax-e #'base)
                                     (string-append (format "~a" (->unsyntax SUFFIX)) ...))])])
      (if got-single? (car result) result)))


(define-macro (suffix-id BASE-OR-BASES SUFFIX ...)
  #'(infix-id "" BASE-OR-BASES SUFFIX ...))


(define-macro-cases syntax-property*
  [(_ STX 'PROP0) ; read one
   #'(syntax-property STX 'PROP0)]
  [(_ STX 'PROP0 'PROP ...) ; read multiple
   #'(cons (syntax-property* STX 'PROP0)
           (let ([result (syntax-property* STX 'PROP ...)])
             (if (pair? result)
                 result
                 (list result))))]
  [(_ STX ['PROP0 VAL0 . PRESERVED0]) ; write one
   #'(syntax-property STX 'PROP0 VAL0 . PRESERVED0)]
  [(_ STX ['PROP0 VAL0 . PRESERVED0] ['PROP VAL . PRESERVED] ...) ; write multiple
   #'(syntax-property* (syntax-property STX 'PROP0 VAL0 . PRESERVED0) ['PROP VAL . PRESERVED] ...)])


(module+ test
  (define x (syntax-property* #'foo ['bar #t] ['zam 'boni]))
  (check-false (syntax-property* x 'foo))
  (check-true (syntax-property* x 'bar))
  (check-equal? (syntax-property* x 'foo 'bar 'zam) '(#f #t boni)))

(define (syntax-srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))
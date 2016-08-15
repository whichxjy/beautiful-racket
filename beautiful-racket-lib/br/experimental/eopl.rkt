#lang br
(require racket/struct (for-syntax br/datum))
(provide define-datatype cases occurs-free?)

(define-macro (define-datatype BASE-TYPE BASE-TYPE-PREDICATE?
                (SUBTYPE [FIELD FIELD-PREDICATE?] ...) ...)
  #'(begin
      (struct BASE-TYPE () #:transparent #:mutable)
      (struct SUBTYPE BASE-TYPE (FIELD ...) #:transparent #:mutable
        #:guard (λ(FIELD ... name)
                  (unless (FIELD-PREDICATE? FIELD)
                    (error name (format "arg ~a is not ~a" FIELD 'FIELD-PREDICATE?))) ...
                  (values FIELD ...))) ...))


(define-datatype lc-exp lc-exp?
  (var-exp [var symbol?])
  (lambda-exp [bound-var symbol?] [body lc-exp?])
  (app-exp [rator lc-exp?] [rand lc-exp?]))


#;(define-syntax (cases stx)
    (syntax-case stx (else)
      [(_ _base-type INPUT-VAR
          [SUBTYPE (POSITIONAL-VAR ...) . _body] ...
          [else . _else-body])
       (inject-syntax ([#'(_subtype? ...) (suffix-id #'(SUBTYPE ...) "?")])
                      #'(cond
                          [(_subtype? INPUT-VAR) (match-let ([(list POSITIONAL-VAR ...) (struct->list INPUT-VAR)])
                                                   . _body)] ...
                                                             [else . _else-body]))]
      [(_ _base-type INPUT-VAR
          SUBTYPE-CASE ...)
       #'(cases _base-type INPUT-VAR
           SUBTYPE-CASE ...
           [else (void)])]))


(define-macro-cases cases
  [(_ BASE-TYPE INPUT-VAR
      [SUBTYPE (POSITIONAL-VAR ...) . BODY] ...
      [else . ELSE-BODY])
   (with-syntax ([(SUBTYPE? ...) (suffix-id #'(SUBTYPE ...) "?")])
     #'(cond
         [(SUBTYPE? INPUT-VAR) (match-let ([(list POSITIONAL-VAR ...) (struct->list INPUT-VAR)])
                                 . BODY)] ...
                                          [else . ELSE-BODY]))]
  [(_ BASE-TYPE INPUT-VAR
      SUBTYPE-CASE ...)
   #'(cases BASE-TYPE INPUT-VAR
       SUBTYPE-CASE ...
       [else (void)])])


(define (occurs-free? search-var exp)
  (cases lc-exp exp
    [var-exp (var) (eqv? var search-var)]
    [lambda-exp (bound-var body)
                (and (not (eqv? search-var bound-var))
                     (occurs-free? search-var body))]
    [app-exp (rator rand)
             (or
              (occurs-free? search-var rator)
              (occurs-free? search-var rand))]))


(module+ test
  (require rackunit)
  (check-true (occurs-free? 'foo (var-exp 'foo)))
  (check-false (occurs-free? 'foo (var-exp 'bar)))
  (check-false (occurs-free? 'foo (lambda-exp 'foo (var-exp 'bar))))
  (check-true (occurs-free? 'foo (lambda-exp 'bar (var-exp 'foo))))
  (check-true (occurs-free? 'foo (lambda-exp 'bar (lambda-exp 'zim (lambda-exp 'zam (var-exp 'foo)))))))
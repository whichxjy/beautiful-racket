#lang br
(require rackunit (for-syntax br/datum sugar/debug))
(provide define-datatype occurs-free?)

#;(begin
    (struct lc-exp () #:transparent)
    
    (struct var-exp lc-exp (var) #:transparent
      #:guard (λ(var name)
                (unless (symbol? var)
                  (error name (format "arg ~a not ~a" var 'symbol?)))
                (values var)))
    
    (struct lambda-exp lc-exp (bound-var body) #:transparent
      #:guard (λ(bound-var body name)
                (unless (symbol? bound-var)
                  (error name (format "arg ~a not ~a" bound-var 'symbol?)))
                (unless (lc-exp? body)
                  (error name (format "arg ~a not ~a" body 'lc-exp?)))
                (values bound-var body)))
    
    (struct app-exp lc-exp (rator rand) #:transparent
      #:guard (λ(rator rand name)
                (unless (lc-exp? rator)
                  (error name (format "arg ~a not ~a" rator 'lc-exp?)))
                (unless (lc-exp? rand)
                  (error name (format "arg ~a not ~a" rand 'lc-exp?)))
                (values rator rand))))


(define #'(define-datatype <base-type> <base-type-predicate?>
            (<subtype> [<field> <field-predicate?>] ...) ...)
  #'(begin
      (struct <base-type> () #:transparent)
      (struct <subtype> <base-type> (<field> ...) #:transparent
        #:guard (λ(<field> ... name)
                  (unless (<field-predicate?> <field>)
                    (error name (format "arg ~a is not ~a" <field> '<field-predicate?>))) ...
                  (values <field> ...))) ...))


(define-datatype lc-exp lc-exp?
  (var-exp [var symbol?])
  (lambda-exp [bound-var symbol?] [body lc-exp?])
  (app-exp [rator lc-exp?] [rand lc-exp?]))


#;(define (occurs-free? search-var exp)
    (cond
      [(var-exp? exp) (let ([var (var-exp-var exp)])
                        (eqv? var  search-var))]
      [(lambda-exp? exp) (let ([bound-var (lambda-exp-bound-var exp)]
                               [body (lambda-exp-body exp)])
                           (and (not (eqv? search-var bound-var))
                                (occurs-free? search-var body)))]
      [(app-exp? exp) (let ([rator (app-exp-rator exp)]
                            [rand (app-exp-rand exp)])
                        (or
                         (occurs-free? search-var rator)
                         (occurs-free? search-var rand)))]))


(define #'(cases-let <input-var> <subtype> (<field> ...) <body> ...)
  (inject-syntax ([#'(<subtype-field> ...) (map-syntax (λ(field) (format-datum '~a-~a #'<subtype> field)) #'(<field> ...))])
                 #'(let ([<field> (<subtype-field> <input-var>)] ...)
                     <body> ...)))


(define #'(cases <base-type> <input-var> [<subtype> (<field> ...) <body> ...] ...)
  (inject-syntax ([#'(<subtype?> ...) (map-syntax (λ(s) (format-datum '~a? s)) #'(<subtype> ...))])
                 #'(cond
                     [(<subtype?> <input-var>) (cases-let <input-var> <subtype> (<field> ...) <body> ...)] ...)))

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



(check-true (occurs-free? 'foo (var-exp 'foo)))
(check-false (occurs-free? 'foo (var-exp 'bar)))
(check-false (occurs-free? 'foo (lambda-exp 'foo (var-exp 'bar))))
(check-true (occurs-free? 'foo (lambda-exp 'bar (var-exp 'foo))))
(check-true (occurs-free? 'foo (lambda-exp 'bar (lambda-exp 'zim (lambda-exp 'zam (var-exp 'foo))))))
#lang racket/base
(require (for-syntax racket/base racket/syntax)
         racket/list
         racket/match
         racket/syntax
         racket/format
         syntax/stx
         syntax/strip-context
         br/define
         br/private/syntax-flatten)
(provide (all-defined-out)
         syntax-flatten
         (rename-out [strip-context strip-bindings]
                     [replace-context replace-bindings]
                     [stx-map syntax-map]))

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
#|
(define (format-string FMT ID0 . IDS)
  (datum->syntax ID0 (apply format FMT (syntax->datum ID0) (map syntax->datum IDS))))
|#


(define (->unsyntax x)
  (if (syntax? x) (syntax->datum x) x))


(define (fix-base loc-arg prefixes base-or-bases suffixes)
  (define single-mode? (and (not (list? base-or-bases)) (not (syntax->list base-or-bases))))
  (define bases (if single-mode? (list base-or-bases) (or (syntax->list base-or-bases) base-or-bases)))
  (define (stx-join stxs) (apply string-append (map (compose1 ~a ->unsyntax) stxs)))
  (define result (map (λ (base) (format-id base "~a~a~a" (stx-join prefixes) (syntax-e base) (stx-join suffixes)
                                           #:source loc-arg)) bases))
  (if single-mode? (car result) result))


(define (prefix-id #:source [loc-arg #f] . args)
  ((match-lambda
     [(list prefixes ... base-or-bases)
      (fix-base loc-arg prefixes base-or-bases empty)]) args))

(define (infix-id #:source [loc-arg #f] . args)
  ((match-lambda
     [(list prefix base-or-bases suffixes ...)
      (fix-base loc-arg (list prefix) base-or-bases suffixes)]) args))

(define (suffix-id #:source [loc-arg #f] . args)
  ((match-lambda
     [(list base-or-bases suffixes ...)
      (fix-base loc-arg empty base-or-bases suffixes)]) args))

(module+ test
  (define-check (check-stx-equal? stx1 stx2)
    (define stxs (list stx1 stx2))
    (apply equal? (map syntax->datum stxs))) 
  (check-stx-equal? (prefix-id "foo" "bar" #'id) #'foobarid)
  (check-stx-equal? (infix-id "foo" #'id "bar" "zam") #'fooidbarzam)
  (check-stx-equal? (suffix-id #'id "foo" "bar" "zam") #'idfoobarzam)
  (for-each check-stx-equal? (suffix-id #'(this that) "@") (list #'this@ #'that@)))


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

#|
(define (syntax-property* . args)
  ((match-lambda*
     [(list x) x]
     [(list stx (list prop val others ...)) ; write one
      (apply syntax-property stx prop val others)]
     [(list stx (list prop val others ...) args ...) ; write multiple
      #'(apply syntax-property* (apply syntax-property stx prop val others) args)]
     [(list stx prop) ; read one
      (syntax-property stx prop)]
     [(list stx prop0 props ...) ; read multiple
      (map (λ (prop) (syntax-property stx prop)) (cons prop0 props))]
     [else 'huh]) args))
|#

(define (filter-stx-prop prop stxs)
  (filter (λ (stx) (syntax-property stx prop)) stxs))

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
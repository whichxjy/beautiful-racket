#lang racket/base
(require (for-syntax
          racket/base
          racket/syntax
          br/private/generate-literals)
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
         stx-map
         (rename-out [strip-context strip-bindings]
                     [replace-context replace-bindings]
                     [stx-map syntax-map]
                     [syntax-flatten stx-flatten]
                     [prefix-id prefix-ids]
                     [suffix-id suffix-ids]
                     [infix-id infix-ids]))

(module+ test
  (require rackunit))


(define-macro-cases case-pattern
  [(_ STX-ARG
      [PAT . BODY] ...
      [else . ELSEBODY]) (with-syntax ([(LITERAL ...) (generate-literals #'(PAT ...))])
                           #'(syntax-case STX-ARG (LITERAL ...)
                               [PAT . BODY] ...
                               [else . ELSEBODY]))]
  [(_ STX-ARG
      PAT+BODY ...) #'(case-pattern STX-ARG
                           PAT+BODY ...
                           [else (raise-syntax-error 'case-pattern
                                                     (format "unable to match pattern for ~v" (syntax->datum STX-ARG)))])])

(define-macro-cases with-pattern
  [(_ () . BODY) #'(begin . BODY)]
  [(_ ([PAT0 STX0] PAT+STX ...) . BODY)
   (with-syntax ([(LITERAL ...) (generate-literals #'PAT0)])
     #'(syntax-case STX0 (LITERAL ...)
         [PAT0 (with-pattern (PAT+STX ...) (let () . BODY))]
         [else (raise-syntax-error 'with-pattern
                                   (format "unable to match pattern ~a" 'PAT0) STX0)]))])


(define-macro (format-string FMT ID0 ID ...)
  #'(datum->syntax ID0 (format FMT (syntax->datum ID0) (syntax->datum ID) ...)))

(define (->unsyntax x) (if (syntax? x) (syntax->datum x) x))

(define (*fix-base loc-arg prefixes base-or-bases suffixes)
  (define single-mode? (and (not (list? base-or-bases)) (not (syntax->list base-or-bases))))
  (define bases (if single-mode? (list base-or-bases) (or (syntax->list base-or-bases) base-or-bases)))
  (define (stx-join stxs) (apply string-append (map (compose1 ~a ->unsyntax) stxs)))
  (define result (map (λ (base) (format-id base "~a~a~a" (stx-join prefixes) (syntax-e base) (stx-join suffixes)
                                           #:source loc-arg)) bases))
  (if single-mode? (car result) result))


(define (prefix-id #:source [loc-arg #f] . args)
  ((match-lambda
     [(list prefixes ... base-or-bases)
      (*fix-base loc-arg prefixes base-or-bases empty)]) args))

(define (infix-id #:source [loc-arg #f] . args)
  ((match-lambda
     [(list prefix base-or-bases suffixes ...)
      (*fix-base loc-arg (list prefix) base-or-bases suffixes)]) args))

(define (suffix-id #:source [loc-arg #f] . args)
  ((match-lambda
     [(list base-or-bases suffixes ...)
      (*fix-base loc-arg empty base-or-bases suffixes)]) args))

(module+ test
  (define-check (check-stx-equal? stx1 stx2)
    (define stxs (list stx1 stx2))
    (apply equal? (map syntax->datum stxs))) 
  (check-stx-equal? (prefix-id "foo" "bar" #'id) #'foobarid)
  (check-stx-equal? (infix-id "foo" #'id "bar" "zam") #'fooidbarzam)
  (check-stx-equal? (suffix-id #'id "foo" "bar" "zam") #'idfoobarzam)
  (for-each check-stx-equal? (suffix-id #'(this that) "@") (list #'this@ #'that@)))


(define (syntax-srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))
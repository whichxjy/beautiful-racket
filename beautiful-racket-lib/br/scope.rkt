#lang racket/base
(require (for-syntax racket/base br/syntax racket/syntax) syntax/strip-context racket/function)
(provide (all-defined-out))

(define (->syntax x)
  (if (syntax? x) x (datum->syntax #f x)))


(define (context stx)
  (hash-ref (syntax-debug-info stx) 'context))

(define-syntax-rule (scopes stx)
  (format "~a = ~a" 'stx
          (cons (syntax->datum stx)
                (for/list ([scope (in-list (context stx))])
                          scope))))

(define (syntax-find stx stx-or-datum)
  (unless (syntax? stx)
    (raise-argument-error 'syntax-find "not given syntax object as first argument" stx))
  (define datum
    (cond [(syntax? stx-or-datum) (syntax->datum stx-or-datum)]
          [(symbol? stx-or-datum) stx-or-datum]
          [else (raise-argument-error 'syntax-find "not given syntax or datum as second argument" stx-or-datum)]))
  (let/ec exit
    (let loop ([so stx])
      (cond
        [(eq? (syntax->datum so) datum) (exit so)]
        [(syntax->list so) => (curry map loop)]))))

(define-syntax (define-scope stx)
  (syntax-case stx ()
    [(_ id)
     #'(define-scope id ())]
    [(_ id scope-ids)
     (with-syntax ([id-sis (suffix-id #'id "-sis")]
                   [add-id (prefix-id "add-" #'id)]
                   [flip-id (prefix-id "flip-" #'id)]
                   [id-binding-form (suffix-id #'id "-binding-form")]
                   [define-id (prefix-id "define-" #'id)]
                   [with-id-identifiers (infix-id "with-" #'id "-identifiers")]
                   [let-id-syntax (infix-id "let-" #'id "-syntax")]
                   [with-id-binding-form (infix-id "with-" #'id "-binding-form")]
                   [remove-id (prefix-id "remove-" #'id)]
                   [id? (suffix-id #'id "?")]
                   [id* (suffix-id #'id "*")]
                   [(scope-id-sis ...) (suffix-ids #'scope-ids "-sis")])
       #'(begin
           (define id-sis
             (let ([sis-in (list scope-id-sis ...)])
               (if (pair? sis-in)
                   (apply append sis-in)
                   (list
                    (let ([si (make-syntax-introducer #t)])
                      (list (procedure-rename (curryr si 'add) 'add-id)
                            (procedure-rename (curryr si 'flip) 'flip-id)
                            (procedure-rename (curryr si 'remove) 'remove-id)))))))
           (define add-id (λ(x) ((apply compose1 (map car id-sis)) (->syntax x))))
           (define flip-id (λ(x) ((apply compose1 (map cadr id-sis)) (->syntax x))))
           (define remove-id (λ(x) ((apply compose1 (map caddr id-sis)) (->syntax x))))
           (define (id x) (add-id (datum->syntax #f (syntax-e (->syntax x)))))
           (define (id-binding-form x) (syntax-local-introduce (id x)))
           (define (id* x) (replace-context (add-id (datum->syntax #f '_)) (->syntax x)))
           (define (id? x)
             (and
              (member (car (context (add-id (datum->syntax #f '_))))
                      (context (->syntax x)))
              #t))
           (define-syntax-rule (with-id-identifiers (name (... ...)) . body)
             (with-syntax ([name (id* 'name)] (... ...)) . body))
           (define-syntax-rule (with-id-binding-form  (name (... ...)) . body)
             (with-syntax ([name (id-binding-form 'name)] (... ...)) . body))
           (define-syntax-rule (let-id-syntax ([pat val] (... ...)) . body)
             (let-syntax ([pat (id* val)] (... ...)) . body))))]))

(define (scopes-equal? stxl stxr)
  ;; "A bound-identifier=? comparison checks that two identifiers have exactly the same scope sets"
  (bound-identifier=? (datum->syntax stxl '_) (datum->syntax stxr '_)))


(module+ test
  (require rackunit)
  (define-scope red)
  
  (define stx (datum->syntax #f 'x))
  
  (define red-stx (add-red stx))
  (define double-red-stx (add-red (add-red stx)))
  
  
  (check-false (red? stx))
  (check-true (red? red-stx))
  (check-true (red? double-red-stx))
  (check-false (scopes-equal? stx red-stx))
  (check-true (scopes-equal? red-stx double-red-stx))
  (check-false (scopes-equal? red-stx (remove-red double-red-stx)))
  
  
  (define-scope blue) ; scope addition is commutative
  (define blue-stx (blue stx))
  (check-true (scopes-equal? (add-blue red-stx) (add-red blue-stx)))
  (check-true (scopes-equal? (remove-red (add-blue red-stx)) (remove-red (add-red blue-stx))))
  
  
  (define-scope green) ; replace scopes at outer layer
  (check-true (scopes-equal? (green red-stx) (green blue-stx)))
  
  
  ;; replace scopes everywhere
  (check-true (scopes-equal? (car (syntax->list (green* #`(#,blue-stx #,red-stx))))
                             (car (syntax->list (green* #`(#,red-stx #,blue-stx))))))
  
  ;; todo: test flipping
  
  
  (define-scope purple (red blue))
  
  (check-true (purple? (add-purple stx)))
  (check-true (scopes-equal? (purple (green stx)) (add-blue (remove-green (add-red (add-green (add-blue stx))))))))


(define-syntax (with-scopes stx)
  (syntax-case stx (syntax)
    [(_ (scope-id) (syntax expr))
     (with-syntax ([add-scope-id (format-id #'scope-id "add-~a" #'scope-id)])
       #'(add-scope-id expr))]))


#lang racket/base
(require (for-syntax racket/base syntax/parse racket/syntax) syntax/strip-context)
(provide (all-defined-out) (all-from-out syntax/strip-context))


(define-syntax (syntax-match stx)
  (syntax-case stx (syntax)
    [(_ stx-arg [(syntax pattern) body ...] ...)
     #'(syntax-case stx-arg ()
         [pattern body ...] ...)]))

(define-syntax (add-syntax stx)
  ;; todo: permit mixing of two-arg and one-arg binding forms
  ;; one-arg form allows you to inject an existing syntax object using its current name
  (syntax-case stx (syntax)
    [(_ ([(syntax sid) sid-stx] ...) body ...)
     #'(with-syntax ([sid sid-stx] ...) body ...)]
    ;; todo: limit `sid` to be an identifier
    [(_ ([sid] ...) body ...)
     #'(with-syntax ([sid sid] ...) body ...)]))

(define-syntax syntax-let (make-rename-transformer #'add-syntax))

(define-syntax inject-syntax (make-rename-transformer #'add-syntax))

(define-syntax (map-syntax stx)
  (syntax-case stx ()
    [(_ <proc> <arg> ...)
     #'(map <proc> (if (and (syntax? <arg>) (list? (syntax-e <arg>)))
                       (syntax->list <arg>)
                       <arg>) ...)]))


#;(define-syntax syntax-variable (make-rename-transformer #'format-id))

(define (context stx)
  (hash-ref (syntax-debug-info stx) 'context))

(define-syntax-rule (scopes stx)
  (format "~a = ~a" 'stx
          (cons (syntax->datum stx)
                (for/list ([scope (in-list (context stx))])
                          scope))))

(define (->syntax x)
  (if (syntax? x) x (datum->syntax #f x)))

(define-syntax (define-scope stx)
  (syntax-case stx ()
    [(_ id)
     #'(define-scope id #f)]
    [(_ id sis)
     (with-syntax ([id-si (format-id #'id "~a-si" #'id)]
                   [add-id (format-id #'id "add-~a" #'id)]
                   [flip-id (format-id #'id "flip-~a" #'id)]
                   [remove-id (format-id #'id "remove-~a" #'id)]
                   [id? (format-id #'id "~a?" #'id)]
                   [id* (format-id #'id "~a*" #'id)])
       #'(begin
           (define id-si (or sis (make-syntax-introducer 'use-site)))
           (define add-id (λ(x) (id-si (->syntax x) 'add)))
           (define flip-id (λ(x) (id-si (->syntax x) 'flip)))
           (define remove-id (λ(x) (id-si (->syntax x) 'remove)))
           (define (id x) (add-id (datum->syntax #f (syntax-e (->syntax x)))))
           (define (id* x) (replace-context (id-si (datum->syntax #f '_)) (->syntax x)))
           (define (id? x)
             (and
              (member (car (context (id-si (datum->syntax #f '_))))
                      (context (->syntax x)))
              #t))))]))


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
  
  ;; todo: scope composition
  #;(define-scope purple (red blue))
  
  #;(check-true (purple? (add-purple stx)))
  #;(scopes-equal? (purple stx) (blue (red stx)))
  
  )

(define-syntax (define-with-scopes stx)
  (syntax-case stx ()
    [(_ id (scope-id) val)
     #'(define-syntax id (λ(stx) (scope-id (syntax val))))]))


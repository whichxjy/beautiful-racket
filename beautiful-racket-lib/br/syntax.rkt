#lang racket/base
(require (for-syntax racket/base syntax/parse racket/syntax syntax/strip-context)
         syntax/strip-context racket/function racket/list racket/syntax)
(provide (all-defined-out) (all-from-out syntax/strip-context))

(module+ test
  (require rackunit))

(define-syntax (syntax-match stx)
  (syntax-case stx (syntax)
    [(_ stx-arg [(syntax pattern) body ...] ...)
     #'(syntax-case stx-arg ()
         [pattern body ...] ...)]))

(define-syntax (inject-syntax stx)
  ;; todo: permit mixing of two-arg and one-arg binding forms
  ;; one-arg form allows you to inject an existing syntax object using its current name
  (syntax-case stx (syntax)
    [(_ ([(syntax sid) sid-stx] ...) body ...)
     #'(inject-syntax ([sid sid-stx] ...) body ...)]
    [(_ ([sid sid-stx] ...) body ...)
     #'(with-syntax ([sid sid-stx] ...) body ...)]
    ;; todo: limit `sid` to be an identifier
    [(_ ([sid] ...) body ...)
     #'(with-syntax ([sid sid] ...) body ...)]))

(define-syntax (inject-syntax* stx)
  (syntax-case stx ()
    [(_ () . body) #'(begin . body)]
    [(_ (stx-expr0 stx-expr ...) . body)
     #'(inject-syntax (stx-expr0)
                      (inject-syntax* (stx-expr ...) . body))]))

(define-syntax with-pattern (make-rename-transformer #'inject-syntax*))
(define-syntax let-syntax-pattern (make-rename-transformer #'inject-syntax*))
(define-syntax let*-syntax-pattern (make-rename-transformer #'inject-syntax*))
(define-syntax syntax-let (make-rename-transformer #'inject-syntax))
(define-syntax add-syntax (make-rename-transformer #'inject-syntax))

(define-syntax-rule (test-macro mac-expr)
  (syntax->datum (expand-once #'mac-expr)))

(define (check-syntax-list-argument caller-name arg)
  (cond
    [(and (syntax? arg) (syntax->list arg))]
    [(list? arg) arg]
    [else (raise-argument-error caller-name "list of syntax, or syntaxed list" arg)]))


(define-syntax-rule (syntax-case-partition _stx-list _literals . _matchers)
  (partition (λ(stx-item)
               (with-handlers ([exn:fail:syntax? (λ (exn) #f)])
                 (syntax-case stx-item _literals
                   . _matchers))) (check-syntax-list-argument 'syntax-case-partition _stx-list)))


(define-syntax-rule (syntax-case-filter _stx-list _literals . _matchers)
  (filter (λ(stx-item)
            (with-handlers ([exn:fail:syntax? (λ (exn) #f)])
              (syntax-case stx-item _literals
                . _matchers))) (check-syntax-list-argument 'syntax-case-filter _stx-list)))


(define-syntax-rule (syntax-case-map _stx-list _literals . _matchers)
  (map (λ(stx-item)
         (syntax-case stx-item _literals
           . _matchers)) (check-syntax-list-argument 'syntax-case-map _stx-list)))


(define-syntax-rule (reformat-id fmt id0 id ...)
  (format-id id0 fmt id0 id ...))

(define-syntax-rule (format-string fmt id0 id ...)
  (datum->syntax id0 (format fmt (syntax->datum id0) (syntax->datum id) ...)))


(define-syntax-rule (->unsyntax x)
  (if (syntax? x)
      (syntax->datum x)
      x))

(define-syntax-rule (prefix-id _prefix ... _base-or-bases)
  (let* ([bob _base-or-bases]
         [got-single? (and (not (list? bob)) (not (syntax->list bob)))]
         [bases (if got-single?
                    (list bob)
                    bob)]
         [result (syntax-case-map
                  bases ()
                  [base (format-id #'base "~a~a"
                                   (string-append (format "~a" (->unsyntax _prefix)) ...)
                                   (syntax-e #'base))])])
    (if got-single? (car result) result)))

(define-syntax-rule (infix-id _prefix _base-or-bases _suffix ...)
  (let* ([bob _base-or-bases]
         [got-single? (and (not (list? bob)) (not (syntax->list bob)))]
         [bases (if got-single?
                    (list bob)
                    bob)]
         [result (syntax-case-map
                  bases ()
                  [base (format-id #'base "~a~a~a" (->unsyntax _prefix) (syntax-e #'base)
                                   (string-append (format "~a" (->unsyntax _suffix)) ...))])])
    (if got-single? (car result) result)))

(define-syntax-rule (suffix-id _base-or-bases _suffix ...)
  (infix-id "" _base-or-bases _suffix ...))

(define-syntax (syntax-property* stx)
  (syntax-case stx (quote)
    [(_ stx-object 'prop0)
     #'(syntax-property stx-object 'prop0)]
    [(_ stx-object 'prop0 'prop ...)
     #'(cons (syntax-property stx-object 'prop0) (let ([result (syntax-property* stx-object 'prop ...)])
                                                   (if (pair? result)
                                                       result
                                                       (list result))))]
    [(_ stx-object ['prop0 val0 . preserved0])
     #'(syntax-property stx-object 'prop0 val0 . preserved0)]
    [(_ stx-object ['prop0 val0 . preserved0] ['prop val . preserved] ...)
     #'(syntax-property* (syntax-property stx-object 'prop0 val0 . preserved0) ['prop val . preserved] ...)]))

(module+ test
  (define x (syntax-property* #'foo ['bar #t] ['zam 'boni]))
  (check-false (syntax-property* x 'foo))
  (check-true (syntax-property* x 'bar))
  (check-equal? (syntax-property* x 'foo 'bar 'zam) '(#f #t boni)))

(define-syntax-rule (introduce-id (id ...) . body)
  (with-syntax ([id (syntax-local-introduce (datum->syntax #f 'id))] ...)
    . body))

(define-syntax with-shared-id (make-rename-transformer #'introduce-id))
(define-syntax mark-as-shared-id (make-rename-transformer #'introduce-id))


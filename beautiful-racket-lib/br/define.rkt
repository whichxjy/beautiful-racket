#lang racket/base
(require (for-syntax racket/list racket/base syntax/parse racket/syntax syntax/datum syntax/strip-context racket/string) sugar/define)
(provide (all-defined-out))

;; everything is prefixed br: whether it needs it or not so it can be stripped by #lang br

(define-for-syntax (syntax-flatten stx)
  (flatten
   (let loop ([stx stx])
     (define maybe-list (syntax->list stx))
     (if maybe-list
         (map loop maybe-list)
         stx))))

(define-for-syntax (generate-literals pats)
  ;; generate literals for any symbols that are not ... or _ or _underscore-prefixed
  (define pattern-arg-prefixer "_")
  (for/list ([pat-arg (in-list (syntax-flatten pats))]
             #:when (let ([pat-datum (syntax->datum pat-arg)])
                      (and (symbol? pat-datum)
                           (not (member pat-datum '(... _ else))) ; exempted from literality
                           (not (string-prefix? (symbol->string pat-datum) pattern-arg-prefixer)))))
            pat-arg))

;; expose the caller context within br:define macros with syntax parameter
(begin-for-syntax
  (require (for-syntax racket/base) racket/stxparam)
  (provide caller-stx shared-syntax)
  (define-syntax-parameter caller-stx (λ(stx) (error 'caller-stx-not-parameterized)))
  (define-syntax-parameter shared-syntax (λ(stx) (error 'shared-syntax-not-parameterized))))


(define-syntax (br:define-cases stx)
  (define-syntax-class syntaxed-id
    #:literals (syntax)
    #:description "id in syntaxed form"
    (pattern (syntax name:id)))
  
  (define-syntax-class syntaxed-thing
    #:literals (syntax)
    #:description "some datum in syntaxed form"
    (pattern (syntax thing:expr)))
  
  (syntax-parse stx
    #:literals (syntax)
    
    ;; defective for syntax or function
    [(_ top-id)
     (raise-syntax-error 'define-cases "no cases given" (syntax->datum #'top-id))]
    
    ;; defective for syntax
    [(_ (sid:syntaxed-id . _) . _)  ; (define (#'f1 stx) expr ...)
     (raise-syntax-error 'define-cases "definition of a syntax transformer must use lambda notation, because otherwise it's too easy to confuse the compile-time shape and the run-time shape" (syntax->datum #'sid.name))]
    
    ;; syntax matcher
    [(_ top-id:syntaxed-id . patexprs)
     ;; todo: rephrase this check as a syntax-parse pattern above
     (let ([all-but-last-pat-datums (map syntax->datum (syntax->list (syntax-case #'patexprs ()
                                                                       [((pat result) ... last-one) #'(pat ...)])))])
       (when (member 'else all-but-last-pat-datums)
         (raise-syntax-error 'define-cases "else case must be last" (syntax->datum #'top-id.name))))
     (with-syntax* ([((pat . result-exprs) ... else-result-exprs)
                     (syntax-case #'patexprs (syntax else)
                       [(((syntax pat) result-expr) ... (else . else-result-exprs))
                        #'((pat result-expr) ... else-result-exprs)]
                       [(((syntax pat) result-expr) ...)
                        #'((pat result-expr) ... (list (raise-syntax-error 'define-cases (format "no matching case for syntax pattern ~v" (syntax->datum stx)) (syntax->datum #'top-id.name))))])]
                    [LITERALS (generate-literals #'(pat ...))])
       #'(define-syntax top-id.name (λ (stx)
                                      (define result
                                        (syntax-case stx LITERALS
                                          [pat (syntax-parameterize ([caller-stx (make-rename-transformer #'stx)])
                                                 (syntax-parameterize ([shared-syntax (make-shared-syntax-macro caller-stx)])
                                                   . result-exprs))] ...
                                          [else . else-result-exprs]))
                                      (if (syntax? result)
                                          result
                                          (datum->syntax #'top-id.name result)))))]
    
    ;; function matcher
    [(_ top-id:id [(_ . pat-args) . body] ...)
     #'(define top-id
         (case-lambda
           [pat-args . body] ...
           [else (raise-syntax-error 'define-cases "no matching case for argument pattern" (object-name top-id))]))]))


(module+ test
  (require rackunit)
  (define foo-val 'got-foo-val)
  (define (foo-func) 'got-foo-func)
  (br:define-cases #'op
                   [#'(_ "+") #''got-plus]
                   [#'(_ _ARG) #''got-something-else]
                   [#'(_) #'(foo-func)]
                   [#'_ #'foo-val])
  
  (check-equal? (op "+") 'got-plus)
  (check-equal? (op 42) 'got-something-else)
  (check-equal? (op) 'got-foo-func)
  (check-equal? op 'got-foo-val)
  
  (br:define-cases #'elseop
                   [#'(_ _arg) #''got-arg]
                   [else #''got-else])
  
  (check-equal? (elseop "+") 'got-arg)
  (check-equal? (elseop "+" 42) 'got-else)
  
  ;; todo: how to check for syntax error?
  ;; `define-cases: else case must be last in: badelseop`
  #;(check-exn exn:fail? (λ _ (br:define-cases #'badelseop
                                               [else #''got-else]
                                               [#'(_ _arg) #''got-arg])))
  
  (br:define-cases f
                   [(_ arg) (add1 arg)]
                   [(_ arg1 arg2) (+ arg1 arg2)])
  
  (check-equal? (f 42) 43)
  (check-equal? (f 42 5) 47))
  
  ;; todo: error from define-cases not trapped by check-exn 
  ;;(check-exn exn:fail:syntax? (λ _ (define-cases (#'times stx stx2) #'*)))




(define-syntax (br:define stx)
  
  ;;todo: share syntax classes
  
  (define-syntax-class syntaxed-id
    #:literals (syntax)
    #:description "id in syntaxed form"
    (pattern (syntax name:id)))
  
  (define-syntax-class syntaxed-thing
    #:literals (syntax)
    #:description "some datum in syntaxed form"
    (pattern (syntax thing:expr)))
  
  (syntax-parse stx
    #:literals (syntax)
    
    ;; syntax
    [(_ (syntax (id . pat-args)) . body) ; (define #'(foo arg) #'(+ arg arg))
     #'(br:define-cases (syntax id) [(syntax (_ . pat-args)) . body])]
    
    [(_ sid:syntaxed-id sid2:syntaxed-id) ; (define #'f1 #'f2)
     #'(define-syntax sid.name (make-rename-transformer sid2))]
    
    [(_ (syntax id) (syntax thing)) ; (define #'f1 #'42)
     #'(br:define-cases (syntax id) [#'_ (syntax thing)])]
    
    [(_ (sid:syntaxed-id stx-arg ...) . exprs)  ; (define (#'f1 stx) expr ...)
     (raise-syntax-error 'define "definition of a syntax transformer must use lambda notation, because otherwise it's too easy to confuse the compile-time shape and the run-time shape" (syntax->datum #'sid.name))]
    
    [(_ sid:syntaxed-id (λ (stx-arg ...) . exprs)) ; (define #'f1 (λ(stx) expr ...)
     #:fail-when (not (= (length (syntax->datum #'(stx-arg ...))) 1))
     (raise-syntax-error 'define "did not get exactly one argument for macro" (syntax->datum #'(stx-arg ...)))
     (with-syntax ([(first-stx-arg other ...) #'(stx-arg ...)])
     #'(define-syntax (sid.name first-stx-arg) . exprs))]
    
    [(_ . args) #'(define . args)]))

(module+ test
  (require rackunit)
  (br:define #'plus (λ(stx) #'+))
  (check-equal? (plus 42) +)
  (br:define #'plusser #'plus)
  (check-equal? (plusser 42) +)
  (check-equal? plusser +)
  (br:define #'(times [nested _ARG]) #'(* _ARG _ARG))
  (check-equal? (times [nested 10]) 100)
  (br:define #'timeser #'times)
  (check-equal? (timeser [nested 12]) 144)
  (br:define #'fortytwo #'42)
  (check-equal? fortytwo 42)
  (check-equal? (let ()
                  (br:define #'(foo _X)
                             (with-syntax ([zam +])
                               #'(zam _X _X))) (foo 42)) 84) 
  ;; todo: error from define not trapped by check-exn 
  #;(check-exn exn:fail:syntax? (λ _ (br:define (#'times stx stx2) #'*)))
  (begin
    (br:define #'(redefine _id) #'(define _id 42))
    (redefine zoombar)
    (check-equal? zoombar 42))
  
  ;; use caller-stx parameter to introduce identifier unhygienically
  (br:define #'(zam _arg1 _arg2 _arg3)
             (with-syntax ([dz (datum->syntax caller-stx 'dirty-zam)])
               #`(define dz 'got-dirty-zam)))
  
  (zam 'this 'that 42)
  (check-equal? dirty-zam 'got-dirty-zam))


(define-syntax-rule (br:debug-define (syntax (id . pat-args)) body-exp)
  (br:define #'(id . pat-args)
             #`(begin
                 (for-each displayln
                           (list
                            (format "input pattern = #'~a" '#,'(id . pat-args))
                            (format "output pattern = #'~a" (cadr '#,'body-exp))
                            (format "invoked as = ~a" (syntax->datum #'(id . pat-args)))
                            (format "expanded as = ~a" '#,(syntax->datum body-exp))
                            (format "evaluated as = ~a" #,body-exp)))
                 #,body-exp)))


(module+ test
  (require rackunit racket/port)
  (parameterize ([current-output-port (open-output-nowhere)])
    (check-equal? (let ()
                    (br:debug-define #'(foo _X _Y _Z)
                                     #'(apply + (list _X _Y _Z)))
                    (foo 1 2 3)) 6)  
    (check-equal? (let ()
                    (br:debug-define #'(foo _X ...) #'(apply * (list _X ...)))
                    (foo 10 11 12)) 1320)))



(define-syntax-rule (br:define+provide . args)
  (define+provide . args))


(define-for-syntax (expand-macro mac)
  (syntax-disarm (local-expand mac 'expression #f) #f))


(define-syntax (br:define-inverting stx)
  (syntax-case stx (syntax)
    [(_ (syntax (_id . _pat-args)) . _syntaxexprs)
     #'(br:define-cases-inverting (syntax _id)
                                  [(syntax (_ . _pat-args)) . _syntaxexprs])]))

(begin-for-syntax
  (begin-for-syntax
    (require (for-syntax racket/base))
    (define-syntax (make-shared-syntax-macro stx)
      (syntax-case stx ()
        [(_ caller-stx)
         #'(λ(stx) (syntax-case stx ()
                     [(_ form)
                      #'(datum->syntax caller-stx (if (syntax? form)
                                                      (syntax-e form)
                                                      form))]))]))))

(define-syntax (br:define-cases-inverting stx)
  (syntax-case stx (syntax)
    [(_ (syntax _id) [(syntax _patarg) . _bodyexprs] ...)
     (with-syntax ([LITERALS (generate-literals #'(_patarg ...))])
       #'(define-syntax (_id stx)
           (syntax-case stx ()
             [(_id . rest)
              (let* ([expanded-stx (with-syntax ([expanded-macros (map expand-macro (syntax->list #'rest))])
                                  #'(_id . expanded-macros))])
                (define result
                  (syntax-case expanded-stx LITERALS
                    [_patarg (syntax-parameterize ([caller-stx (make-rename-transformer #'stx)])
                            (syntax-parameterize ([shared-syntax (make-shared-syntax-macro caller-stx)])
                              . _bodyexprs))] ...
                    [else (raise-syntax-error 'define-cases-inverting (format "no matching case for syntax pattern ~v" (syntax->datum stx)) (syntax->datum #'_id))]))
                (if (syntax? result)
                    result
                    (datum->syntax #'_id result)))])))]))


(module+ test
  ;; an inverting macro expands its arguments.
  ;; so `foo` does not get `(falsy a) (falsy b) (falsy c)` as arguments,
  ;; but rather the result of their expansion, namely `((#f a) (#f b) (#f c))`
  ;; and `tree` does not get `(foo (#f a) (#f b) (#f c))` as its first argument,
  ;; but rather the result of its expansion, namely (a b c).
  (br:define-inverting #'(tree (_id ...) _vals)
                       #'(let ()
                           (define-values (_id ...) _vals)
                           (list _id ...)))
  
  (br:define-cases-inverting #'foo
                             [#'(_ (#f _id) ...) #'(_id ...)])
  
  (define-syntax-rule (falsy id) (#f id))
  
  (check-equal? (tree (foo (falsy a) (falsy b) (falsy c)) (values 1 2 3)) '(1 2 3)))
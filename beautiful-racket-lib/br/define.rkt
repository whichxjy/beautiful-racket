#lang racket/base
(require
  (for-syntax racket/list
              racket/base
              syntax/parse
              br/syntax
              racket/syntax
              syntax/datum
              racket/string))
(provide (all-defined-out)
         (for-syntax with-shared-id with-calling-site-id))

(module+ test
  (require rackunit))

(define-for-syntax (upcased? str) (equal? (string-upcase str) str))

(define-for-syntax (generate-literals pats)
  ;; generate literals for any symbols that are not ... or _ or _underscore-prefixed
  (define pattern-arg-prefixer "_")
  (for/list ([pat-arg (in-list (syntax-flatten pats))]
             #:when (let ([pat-datum (syntax->datum pat-arg)])
                      (and (symbol? pat-datum)
                           (not (member pat-datum '(... _ else))) ; exempted from literality
                           (not (string-prefix? (symbol->string pat-datum) pattern-arg-prefixer))
                           (not (upcased? (symbol->string pat-datum))))))
            pat-arg))

;; expose the caller context within br:define macros with syntax parameter
(begin-for-syntax
  (require (for-syntax racket/base) racket/stxparam)
  (provide caller-stx shared-syntax)
  (define-syntax-parameter caller-stx (λ(stx) (error 'caller-stx-not-parameterized)))
  (define-syntax-parameter shared-syntax (λ(stx) (error 'shared-syntax-not-parameterized))))


(define-syntax (define-cases stx)
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
                     (syntax-parse #'patexprs
                       #:literals (syntax else)
                       ;; syntax notation on pattern is optional
                       [(((~or (syntax pat) pat) result-expr) ... (else . else-result-exprs))
                        #'((pat result-expr) ... else-result-exprs)]
                       [(((~or (syntax pat) pat) result-expr) ...)
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
  (define-cases f
    [(_ arg) (add1 arg)]
    [(_ arg1 arg2) (+ arg1 arg2)])
  (check-equal? (f 42) 43)
  (check-equal? (f 42 5) 47))


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
     #'(define-cases (syntax id) [(syntax (_ . pat-args)) (begin . body)])]
    
    [(_ sid:syntaxed-id sid2:syntaxed-id) ; (define #'f1 #'f2)
     #'(define-syntax sid.name (make-rename-transformer sid2))]
    
    [(_ (syntax id) (syntax thing)) ; (define #'f1 #'42)
     #'(define-cases (syntax id) [#'_ (syntax thing)])]
    
    [(_ (sid:syntaxed-id stx-arg ...) . exprs)  ; (define (#'f1 stx) expr ...)
     (raise-syntax-error 'define "definition of a syntax transformer must use lambda notation, because otherwise it's too easy to confuse the compile-time shape and the run-time shape" (syntax->datum #'sid.name))]
    
    [(_ sid:syntaxed-id (λ (stx-arg ...) . exprs)) ; (define #'f1 (λ(stx) expr ...)
     #:fail-when (not (= (length (syntax->datum #'(stx-arg ...))) 1))
     (raise-syntax-error 'define "did not get exactly one argument for macro" (syntax->datum #'(stx-arg ...)))
     (with-syntax ([(first-stx-arg other ...) #'(stx-arg ...)])
       #'(define-syntax (sid.name first-stx-arg) . exprs))]
    
    [(_ . args) #'(define . args)]))


(define-syntax-rule (debug-define-macro (id . pat-args) body-exp)
  (define-macro (id . pat-args)
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
                    (debug-define-macro (foo _X _Y _Z)
                                        #'(apply + (list _X _Y _Z)))
                    (foo 1 2 3)) 6)  
    (check-equal? (let ()
                    (debug-define-macro (foo _X ...) #'(apply * (list _X ...)))
                    (foo 10 11 12)) 1320)))



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

(begin-for-syntax
  (define-syntax-rule (with-shared-id (id ...) . body)
    (with-syntax ([id (shared-syntax 'id)] ...)
      . body))
  
  (define-syntax with-calling-site-id (make-rename-transformer #'with-shared-id)))

(define-syntax (define-macro stx)
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
    [(_ id #'other-id) ; (define-macro id #'other-id)
     #'(br:define #'id #'other-id)]
    [(_ (id . patargs) . body)
     #'(br:define #'(id . patargs) . body)]
    [(_ id [pat . patbody] ...)
     #'(define-cases (syntax id) [pat . patbody] ...)]))

(define-syntax (define-macro-cases stx)
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
    [(_ id . body)
     #'(define-cases (syntax id) . body)]))


(module+ test
  ;; todo: make these tests work, if they still make sense
  #;(define-macro plus (λ(stx) #'+))
  #;(check-equal? (plus 42) +)
  #;(define-macro plusser #'plus)
  #;(check-equal? (plusser 42) +)
  #;(check-equal? plusser +)
  (define-macro (times [nested ARG]) #'(* ARG ARG))
  (check-equal? (times [nested 10]) 100)
  (define-macro timeser #'times)
  (check-equal? (timeser [nested 12]) 144)
  (define-macro fortytwo #'42)
  (check-equal? fortytwo 42)
  (check-equal? (let ()
                  (define-macro (foo X)
                    (with-syntax ([zam +])
                      #'(zam X X))) (foo 42)) 84) 
  (begin
    (define-macro (redefine ID) #'(define ID 42))
    (redefine zoombar)
    (check-equal? zoombar 42))
  
  ;; use caller-stx parameter to introduce identifier unhygienically
  (define-macro (zam ARG1 ARG2 ARG3)
    (with-syntax ([dz (datum->syntax caller-stx 'dirty-zam)])
      #`(define dz 'got-dirty-zam)))
  
  (zam 'this 'that 42)
  (check-equal? dirty-zam 'got-dirty-zam)
  
  (define-macro (add _x) #'(+ _x _x))
  (check-equal? (add 5) 10)
  (define-macro-cases add-again [(_ X) #'(+ X X)])
  (check-equal? (add-again 5) 10)
  (define-macro add-3rd [(_ X) #'(+ X X)])
  (check-equal? (add-3rd 5) 10)
  (define-macro add-4th #'add-3rd)
  (check-equal? (add-4th 5) 10)
  (define foo-val 'got-foo-val)
  (define (foo-func) 'got-foo-func)
  (define-macro-cases op
    [(_ "+") #''got-plus]
    [(_ _ARG) #''got-something-else]
    [(_) #'(foo-func)]
    [_ #'foo-val])
  
  (check-equal? (op "+") 'got-plus)
  (check-equal? (op 42) 'got-something-else)
  (check-equal? (op) 'got-foo-func)
  (check-equal? op 'got-foo-val)
  
  (define-macro-cases elseop
    [(_ _arg) #''got-arg]
    [else #''got-else])
  
  (check-equal? (elseop "+") 'got-arg)
  (check-equal? (elseop "+" 42) 'got-else)

  ;; todo: fix test, should throw error because `else` clause is out of order
  #;(check-exn exn:fail:syntax? (λ _ (expand-once #'(define-macro-cases badelseop
                                                      [else #''got-else]
                                                      [(_ _arg) #''got-arg])))))
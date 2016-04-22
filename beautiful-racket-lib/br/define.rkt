#lang racket/base
(require (for-syntax racket/list racket/base syntax/parse racket/syntax syntax/datum syntax/strip-context) sugar/define)
(provide (all-defined-out))

;; everything is prefixed br: whether it needs it or not so it can be stripped by #lang br

(define-for-syntax (generate-literals pats)
  ;; generate literals for any symbols that are not ... or _ or _underscore-prefixed
  (for*/list ([pat-arg (in-list (flatten (map (λ(stx) (or (syntax->list stx) stx)) (syntax->list pats))))]
              [pat-datum (in-value (syntax->datum pat-arg))]
              #:when (and (symbol? pat-datum)
                          (not (eq? pat-datum '...)) (not (eq? pat-datum '_))
                          (not (let ([str (symbol->string pat-datum)])
                                 (regexp-match #rx"^_" str)))))
             pat-arg))

;; todo: support `else` case
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
    [(_ (sid:syntaxed-id _ ...) _ ...)  ; (define (#'f1 stx) expr ...)
     (raise-syntax-error 'define-cases "definition of a syntax transformer must use lambda notation, because otherwise it's too easy to confuse the compile-time shape and the run-time shape" (syntax->datum #'sid.name))]
    
    ;; syntax matcher
    [(_ top-id:syntaxed-id [(syntax pat) body ...] ...+)
     (with-syntax ([(LITERAL ...) (generate-literals #'(pat ...))])
       #'(define-syntax top-id.name (λ (stx)
                                      (define result
                                        (syntax-case stx (LITERAL ...)
                                          [pat body ...] ...
                                          [else (raise-syntax-error 'define-cases (format "no matching case for syntax pattern `~a`" (syntax->datum stx)) (syntax->datum #'top-id.name))]))
                                      (if (not (syntax? result))
                                          (datum->syntax stx result)
                                          result))))]
    
    ;; function matcher
    [(_ top-id:id [(_ pat-arg ... . rest-arg) body ...] ...)
     #'(define top-id
         (case-lambda
           [(pat-arg ... . rest-arg) body ...] ...
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
  
  (br:define-cases f
                   [(_ arg) (add1 arg)]
                   [(_ arg1 arg2) (+ arg1 arg2)])
  
  (check-equal? (f 42) 43)
  (check-equal? (f 42 5) 47)
  
  ;; todo: error from define-cases not trapped by check-exn 
  ;;(check-exn exn:fail:syntax? (λ _ (define-cases (#'times stx stx2) #'*)))
  
  )



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
    [(_ (syntax (id pat-arg ... . rest-arg)) body ...) ; (define #'(foo arg) #'(+ arg arg))
     #'(br:define-cases (syntax id) [(syntax (_ pat-arg ... . rest-arg)) body ...])]
    
    [(_ sid:syntaxed-id sid2:syntaxed-id) ; (define #'f1 #'f2)
     #'(define-syntax sid.name (make-rename-transformer sid2))]
    
    [(_ (syntax id) (syntax thing)) ; (define #'f1 #'42)
     #'(br:define-cases (syntax id) [#'_ (syntax thing)])]
    
    [(_ (sid:syntaxed-id stx-arg ...) expr ...)  ; (define (#'f1 stx) expr ...)
     (raise-syntax-error 'define "definition of a syntax transformer must use lambda notation, because otherwise it's too easy to confuse the compile-time shape and the run-time shape" (syntax->datum #'sid.name))]
    
    [(_ sid:syntaxed-id (λ (stx-arg ...) expr ...)) ; (define #'f1 (λ(stx) expr ...)
     #:fail-when (not (= (length (syntax->datum #'(stx-arg ...))) 1))
     (raise-syntax-error 'define "did not get exactly one argument for macro" (syntax->datum #'(stx-arg ...)))
     #'(define-syntax (sid.name stx-arg ...) expr ...)]
    
    [(_ args ...) #'(define args ...)]))

(module+ test
  (require rackunit)
  (br:define #'plus (λ(stx) #'+))
  (check-equal? (plus 42) +)
  (br:define #'plusser #'plus)
  (check-equal? (plusser 42) +)
  (check-equal? plusser +)
  (br:define #'(times _ARG) #'(* _ARG _ARG))
  (check-equal? (times 10) 100)
  (br:define #'timeser #'times)
  (check-equal? (timeser 12) 144)
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
    (check-equal? zoombar 42)))


(define-syntax-rule (br:debug-define (syntax (id pat-arg ... . rest-arg)) body-exp)
  (br:define #'(id pat-arg ... . rest-arg)
             #`(begin
                 (for-each displayln
                           (list
                            (format "input pattern = #'~a" '#,'(id pat-arg ... . rest-arg))
                            (format "output pattern = #'~a" (cadr '#,'body-exp))
                            (format "invoked as = ~a" (syntax->datum #'(id pat-arg ... . rest-arg)))
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



(define-syntax-rule (br:define+provide arg ...)
  (define+provide arg ...)) 

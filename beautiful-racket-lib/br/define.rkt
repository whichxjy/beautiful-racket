#lang racket/base
(require (for-syntax racket/base syntax/parse racket/syntax syntax/datum syntax/strip-context) sugar/define)
(provide (all-defined-out))


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
                    (br:debug-define #'(foo <x> <y> <z>)
                                     #'(apply + (list <x> <y> <z>)))
                    (foo 1 2 3)) 6)  
    (check-equal? (let ()
                    (br:debug-define #'(foo <x> ...) #'(apply * (list <x> ...)))
                    (foo 10 11 12)) 1320)))


(define-syntax (br:define stx)
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
    [(_ (syntax (id pat-arg ... . rest-arg)) body ...) ; (define #'(foo arg) #'(+ arg arg))
     #'(define-syntax id (λ (stx)
                           (define result
                             (syntax-case stx ()
                               [(_ pat-arg ... . rest-arg) body ...]))
                           (if (not (syntax? result))
                               (datum->syntax stx result)
                               result)))]
    
    [(_ sid:syntaxed-id sid2:syntaxed-id) ; (define #'f1 #'f2)
     #'(define-syntax sid.name (make-rename-transformer sid2))]

    [(_ sid:syntaxed-id sid2:syntaxed-thing) ; (define #'f1 #'42)
     #'(define-syntax sid.name (λ (stx) sid2))]
    
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
  (br:define #'plusser #'plus)
  (br:define #'(times arg) #'(* arg arg))
  (br:define #'timeser #'times)
  (br:define #'fortytwo #'42)
  (check-equal? (plus 42) +)
  (check-equal? plusser +)
  (check-equal? (plusser 42) +)
  (check-equal? (times 10) 100)
  (check-equal? (timeser 12) 144)
  (check-equal? (let ()
                  (br:define #'(foo x)
                             (with-syntax ([zam +])
                               #'(zam x x))) (foo 42)) 84) 
  ;; todo: error from define not trapped by check-exn 
  #;(check-exn exn:fail:syntax? (λ _ (br:define (#'times stx stx2) #'*)))
  (check-equal? fortytwo 42))


;; todo: support `else` case
(define-syntax (br:define-cases stx)
  (syntax-parse stx
    #:literals (syntax)
    ; (define-cases #'foo [#'(_ arg) #'(+ arg arg)] [#'(_ 42 bar) #'42] ...)
    [(_ (syntax top-id) [(syntax (_ pat-arg ... . rest-arg)) body ...] ...) 
     #'(define-syntax top-id (λ (stx)
                               (define result
                                 (syntax-case stx ()
                                   [(_ pat-arg ... . rest-arg) body ...] ...))
                               (if (not (syntax? result))
                                   (datum->syntax stx result)
                                   result)))]
    
    [(_ top-id [(_ pat-arg ... . rest-arg) body ...] ...)
     #'(define top-id
         (case-lambda
           [(pat-arg ... . rest-arg) body ...] ...))]))

(module+ test
  (br:define-cases #'op
    [#'(_ "+") #''got-plus]
    [#'(_ arg) #''got-something-else])

  (check-equal? (op "+") 'got-plus)
  (check-equal? (op 42) 'got-something-else)
  
  (br:define-cases f
    [(_ arg) (add1 arg)]
    [(_ arg1 arg2) (+ arg1 arg2)])

  (check-equal? (f 42) 43)
  (check-equal? (f 42 5) 47))


(define-syntax-rule (br:define+provide arg ...)
  (define+provide arg ...)) 


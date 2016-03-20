#lang racket/base
(require (for-syntax racket/base syntax/parse racket/syntax syntax/datum syntax/strip-context))
(provide (all-defined-out))


;; a little tricky because we have to mix two levels of macrology.
(define-syntax (br:debug-define stx)
  (syntax-parse stx
    #:literals (syntax)
    [(_ (syntax (id pat-arg ... . rest-arg)) body-exp) ; (define #'(foo arg) #'(+ arg arg))
     #'(define-syntax id (λ (stx)
                           (define result (syntax-case stx ()
                                            [(_ pat-arg ... . rest-arg)
                                             body-exp]))
                           (define arg-printing (syntax-case stx ()
                                                  [(_ pat-arg ... . rest-arg)
                                                   #`(begin
                                                       (displayln (format "arg #'~a = ~a" #,''pat-arg pat-arg)) ...)]))
                           (with-syntax ([syntaxed-arg-printing arg-printing]
                                         [syntaxed-result result])
                             #'(begin
                                 (displayln (format "input syntax = #'~a" (quote (id pat-arg ... . rest-arg))))
                                 (displayln (format "output syntax = #'~a" (syntax->datum body-exp)))
                                 syntaxed-arg-printing
                                 (displayln (format "expanded syntax = #'~a" 'syntaxed-result))
                                 syntaxed-result))))]))

(module+ test
  (require rackunit racket/port)
  (check-equal? (parameterize ([current-output-port (open-output-nowhere)])
                  (br:debug-define #'(foo <x> <y> <z>)
                                   #'(apply + (list <x> <y> <z>)))
                  (foo 1 2 3)) 6))

;; does not work with ellipses in the input pattern
#;(br:debug-define #'(foo <x> ...)
                   #'(apply + (list <x> ...)))

(define-syntax (br:define stx)
  (define-syntax-class syntaxed-id
    #:literals (syntax)
    #:description "id in syntaxed form"
    (pattern (syntax name:id)))
  
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
  #;(check-exn exn:fail:syntax? (λ _ (br:define (#'times stx stx2) #'*))))

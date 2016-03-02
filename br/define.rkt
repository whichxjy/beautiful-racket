#lang racket/base
(require (for-syntax racket/base syntax/parse))
(provide (all-defined-out))

(define-syntax (br:define stx)
  (define-syntax-class syntaxed-id
    #:literals (syntax)
    #:description "id in syntaxed form"
    (pattern (syntax name:id)))
  
  (syntax-parse stx
    #:literals (syntax)
    [(_ (syntax (id pat-arg ...)) (syntax body ...)) ; (define #'(foo arg) #'(+ arg arg))
     #'(define-syntax-rule (id pat-arg ...) body ...)]
    
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
  ;; todo: error from define not trapped by check-exn 
  #;(check-exn exn:fail:syntax? (λ _ (br:define (#'times stx stx2) #'*))))
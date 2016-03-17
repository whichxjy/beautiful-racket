#lang racket/base
(require (for-syntax racket/base syntax/parse #;racket/syntax) #;racket/syntax)
(provide (all-defined-out))

(define-syntax (syntax-match stx)
  (syntax-case stx (syntax)
    [(_ stx-arg [(syntax pattern) body ...] ...)
     #'(syntax-case stx-arg ()
         [pattern body ...] ...)]))

(define-syntax (add-syntax stx)
  (syntax-case stx (syntax)
    [(_ ([(syntax sid) sid-stx] ...) body ...)
     #'(with-syntax ([sid sid-stx] ...) body ...)]))

(define-syntax syntax-let (make-rename-transformer #'add-syntax))

(define-syntax inject-syntax (make-rename-transformer #'add-syntax))

(define-syntax (map-syntax stx)
  (syntax-case stx ()
    [(_ <proc> <arg> ...)
     #'(map <proc> (if (and (syntax? <arg>) (list? (syntax-e <arg>)))
                       (syntax->list <arg>)
                       <arg>) ...)]))


#;(define-syntax syntax-variable (make-rename-transformer #'format-id))
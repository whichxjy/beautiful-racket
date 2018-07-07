#lang racket
(provide #%datum #%top-interaction #%module-begin
         (rename-out [#%my-app #%app]))

(define-syntax (#%datum stx)
  (syntax-case stx ()
    [(_ . THING) #''taco]))

(define-syntax (#%my-app stx)
  (syntax-case stx ()
    [(_ FUNC . ARGS) #'(list (#%datum) . ARGS)]))

(module reader syntax/module-reader
  atomic-taco-demo)
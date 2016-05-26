#lang racket
(require (for-syntax racket/syntax))

(module pred racket
  (provide pred?)
  (define (pred? x) (zero? (modulo x 7))))

(require 'pred)
(require (for-syntax 'pred))

(define val 42)
(define-for-syntax val 43)

(define-syntax (foo stx)
  (syntax-case stx ()
    [(_) (if (syntax-local-eval (syntax-shift-phase-level #'(pred? val) 0))
             #''yay
             #''boo)]))

(foo)


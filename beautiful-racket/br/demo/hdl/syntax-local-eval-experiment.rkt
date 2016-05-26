#lang br
(require (for-syntax racket/syntax) rackunit)

(module pred racket
  (provide pred? val)
  (define val 43)
  (define (pred? x) (zero? (modulo x 7))))

(require 'pred)


(define-syntax (foo stx)
  (syntax-case stx ()
    [(_) #'(if (pred? val)
               'yay
               'boo)]))

(check-equal? (foo) 'boo)


(define-syntax (foo2 stx)
  (syntax-case stx ()
    [(_)
     (let ()
       (local-require (submod "." pred))
       (if (syntax-local-eval (syntax-shift-phase-level #'(pred? val) 1))
             #''yay
             #''boo))]))

(check-equal? (foo2) 'boo)


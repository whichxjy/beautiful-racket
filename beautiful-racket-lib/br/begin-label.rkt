#lang br
(require rackunit)
(provide (all-defined-out))

(define (syntax->source stx)
  ;; reconstitute the source string by using srclocs
  ;; magic goes here
    stx)

(define-macro (begin-label LABEL . EXPRS)
  #'(begin
      (define LABEL (syntax->source #'EXPRS))
      (provide LABEL)
      (begin . EXPRS)))

(begin-label
  zing
  (define (f x)
    (+ x x))
  
  (define (g x)
    (* x x)))

(display zing)

(check-equal? (f 5) 10)
#;(check-equal? (g 5) 25)
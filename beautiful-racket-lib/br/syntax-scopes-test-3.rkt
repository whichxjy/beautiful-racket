#lang br
(require "subscope.rkt")

(introduce-scope blue)
(introduce-scope red)

(define #'(double-x)
  (with-blue-identifiers (x)
                         #'(set! x (+ x x))))

(define #'(display-x)
  (with-blue-identifiers (x)
                         #'(displayln x)))


(blue:define x 42)

blue:x

(double-x)

(display-x)
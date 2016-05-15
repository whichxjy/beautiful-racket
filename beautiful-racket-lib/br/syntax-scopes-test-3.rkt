#lang br
(require "subscope.rkt")

(introduce-scope blue)
(introduce-scope red)

#;(introduce-scope purple (red blue))


(define #'(double-x)
  (with-blue-identifiers (x)
                         #'(+ x x)))


(define:blue x 50)
x:blue

(define:red x 42)
x:red

(double-x)
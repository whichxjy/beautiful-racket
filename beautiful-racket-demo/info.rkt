#lang info
;; the subdirectories here are suffixed with -demo
;; so they don't collide in the #lang namespace
;; with the test projects that readers will be making themselves

(define collection 'multi)

(define version "1.5")

(define deps '(["base" #:version "6.6.0.900"]
               "sugar"
               "beautiful-racket-lib"
               "rackunit-lib"
               "brag"
               "srfi-lib"
               "draw-lib"
               "syntax-color-lib"
               "gui-lib"
               "math-lib"))

#lang info
(define collection 'multi)

(define version "0.01")
(define deps '("base"
               "sugar"
               ["gui-lib" #:version "1.26"])) ; for indenter fix
(define build-deps '("racket-doc" "rackunit-lib" "scribble-lib"))

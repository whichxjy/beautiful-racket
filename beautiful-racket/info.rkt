#lang info
(define collection 'multi)

(define version "1.4")

(define deps '("beautiful-racket-lib"
               "beautiful-racket-demo"))

(define build-deps '("at-exp-lib"
                     "br-parser-tools-doc"
                     "racket-doc"
                     "scribble-lib"))

(define implies deps)
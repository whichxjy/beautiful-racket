#lang info
(define collection 'multi)

(define version "1.5")

(define deps '(["base" #:version "6.7"]
               "beautiful-racket-lib"
               "beautiful-racket-demo"))

(define build-deps '("gui-doc"
                     "gui-lib"
                     "at-exp-lib"
                     "br-parser-tools-doc"
                     "racket-doc"
                     "scribble-lib"))

(define implies '("beautiful-racket-lib"
                  "beautiful-racket-demo"))
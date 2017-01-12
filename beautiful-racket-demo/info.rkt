#lang info
;; the subdirectories here are suffixed with -demo
;; so they don't collide in the #lang namespace
;; with the test projects that readers will be making themselves

(define collection 'multi)

(define version "0.01")
(define deps '("base" "sugar" "beautiful-racket-lib" "rackunit-lib" "brag" "parser-tools-lib" "srfi-lib" "draw-lib" "syntax-color-lib" "gui-lib"))
(define build-deps '("racket-doc" "scribble-lib"))
(define compile-omit-paths 'all)
(define test-omit-paths 'all)
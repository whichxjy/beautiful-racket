#lang info
(define collection 'multi)

(define version "0.01")
(define deps '("base" "sugar" "beautiful-racket-lib" "rackunit-lib" "ragg" "parser-tools-lib"))
(define build-deps '("racket-doc"))

(define compile-omit-paths '("br/demo"))
(define test-omit-paths '("br/demo"))
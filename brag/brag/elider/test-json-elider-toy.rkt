#lang br
(require "json-elider-toy.rkt"
         brag/support
         rackunit)

(check-equal?
 (syntax->datum
  (parse (list "bar")))
 '(thing))

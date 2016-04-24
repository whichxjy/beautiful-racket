#lang br
(provide (all-from-out br) (all-defined-out))

;; todo: extract identifiers from _pin-spec
;; and introduce them
(define #'(chip-program "CHIP" _id "{" _pin-spec "}")
  #'(begin
      (define _id 0)
      ))
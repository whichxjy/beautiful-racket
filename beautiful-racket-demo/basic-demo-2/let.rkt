#lang br
(provide b-let)
(define-macro (b-let ID VAL) #'(set! ID VAL))
#lang br/quicklang
(provide + *)

(define-macro (stackerizer-mb EXPR ...)
  #'(#%module-begin
     (for-each displayln (reverse (flatten EXPR ...)))))
(provide (rename-out [stackerizer-mb #%module-begin]))

(define-macro (define-ops OP ...)
  #'(begin
      (define-macro-cases OP
        [(OP FIRST) #'FIRST]
        [(OP FIRST NEXT (... ...))
         #'(list 'OP FIRST (OP NEXT (... ...)))]) 
      ...))

(define-ops + *)

(module+ test 
  (require rackunit)
  (check-equal? (with-output-to-string (λ () (dynamic-require "stackerizer-test.rkt" #f)))
                "4
8
+
3
*
"))
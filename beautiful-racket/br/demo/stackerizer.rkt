#lang br/quicklang
(provide + *)

(define-macro (stackerizer-module-begin EXPR ...)
  #'(#%module-begin
     (for-each displayln (reverse (flatten (list EXPR ...))))))
(provide (rename-out [stackerizer-module-begin #%module-begin]))

#|
(define-macro-cases *
  [(*) #'1]
  [(* EXPR0) #'EXPR0]
  [(* EXPR0 EXPR ...) #'(list '* EXPR0 (* EXPR ...))])

(define-macro-cases +
  [(+) #'0]
  [(+ EXPR0) #'EXPR0]
  [(+ EXPR0 EXPR ...) #'(list '+ EXPR0 (+ EXPR ...))])
|#


(define-macro (define-op-macro OP-NAME IDENTITY-VAL)
  #'(define-macro-cases OP-NAME
      [(OP-NAME) #'IDENTITY-VAL]
      [(OP-NAME EXPR0) #'EXPR0]
      [(OP-NAME EXPR0 EXPR (... ...)) #'(list 'OP-NAME EXPR0 (OP-NAME EXPR (... ...)))]))

(define-op-macro * 1)
(define-op-macro + 0)
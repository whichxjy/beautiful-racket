#lang racket/base
(require br/define (for-syntax racket/base))
(provide (all-defined-out))
         
(define-macro (values->list EXPR)
  #'(call-with-values (λ () EXPR) list))

#lang br
(require (for-syntax br/syntax))

(begin-for-syntax
  (define-scope blue))

(define #'(define-blue _id _expr)
  (with-syntax ([_id (blue-binding-form #'_id)])
    #'(define _id _expr)))

(define #'(blue _id)
  (with-syntax ([_id (blue #'_id)])
    #'_id))

(define-syntax x:blue (make-rename-transformer (with-syntax ([x (blue #'x)])
                                                 #'x)))

(define-blue x (+ 42 42))
(define y 50)

#;(+ (blue x) y)
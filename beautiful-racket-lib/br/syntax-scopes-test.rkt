#lang br
(require (for-syntax br/syntax sugar/debug) br/syntax)

(begin-for-syntax
  (define-scope blue)
  (define-scope yellow)
  (define-scope red)
  (define-scope green (blue yellow))
  (define-scope purple (blue red)))

(define #'(define-blue _id _val)
  (with-blue-binding-form ([x '_id])
    #'(define x _val)))

#;(define-blue x (+ 42 42))

(define #'(def-x)
  (with-blue-binding-form ([x 'x])
    #'(define x (+ 42 42))))

(define #'(def-x-2)
  (with-yellow-binding-form ([x 'x])
    #'(define x (+ 42))))

(define #'(print-x)
  (with-yellow-syntax ([x 'x])
                      #'(println (+ x x))))

(define #'(print-x-2)
  (with-purple-syntax ([x 'x])
                      #'(println (+ x x x))))


(scopes (syntax-find (expand-once #'(def-x)) 'x))
(def-x)
(def-x-2)
(scopes (syntax-find (expand-once #'(print-x)) 'x))
(print-x)
(scopes (syntax-find (expand-once #'(print-x-2)) 'x))
(print-x-2)

#;(let-syntax ([x (λ(stx) (syntax-case stx () [_ #'42]))])
  (* x 4))

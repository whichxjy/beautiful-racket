#lang br
(require (for-syntax br/syntax sugar/debug) br/syntax)

(begin-for-syntax
  (define-scope blue)
  (define-scope yellow)
  (define-scope red)
  (define-scope green (blue yellow))
  (define-scope purple (blue red)))

(define #'(def-blue-x)
  (with-blue-binding-form (x)
                          #'(define x (+ 42 42))))


(define #'(print-blue-x)
  (with-purple-identifiers (x)
                           #'x))


(define #'(define-blue _id _expr)
  (with-syntax ([_id (blue-binding-form #'_id)])
    #'(define _id _expr)))



(define #'(print-blue-y)
  (with-blue-identifiers (y)
                         #'y))

(scopes (syntax-find (expand-once #'(def-blue-x)) 'x))
(def-blue-x)
(scopes (syntax-find (expand-once #'(print-blue-x)) 'x))
(print-blue-x)
(let ()
  (scopes (syntax-find (expand-once #'(print-blue-x)) 'x))
  #;(print-blue-x)) ;; error why?

(define-blue y (+ 42 42))
(print-blue-y)

#|
(define #'(def-y)
  (with-yellow-binding-form (y)
                            #'(define y (+ 42))))




#;(scopes (syntax-find (expand-once #'(def-x)) 'x))
#;(def-x)
(def-y)
(scopes (syntax-find (expand-once #'(print-x)) 'x))
(print-x)
(scopes (syntax-find (expand-once #'(print-y)) 'y))
(print-y)

#;(let-syntax ([x (λ(stx) (syntax-case stx () [_ #'42]))])
    (* x 4))

|#

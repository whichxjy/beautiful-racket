#lang br
(require "go.rkt")
(provide b-if b-comp-expr b-logic-expr)


#|
explain why this won't work due to premature eval of THEN & ELSE
(define (b-if COND THEN ELSE)
  (let ([result (if (not (zero? COND))
                     THEN
                     ELSE)])
    (when (exact-positive-integer? result)
      (b-goto result))))
|#

(define-macro-cases b-if
  [(_ COND THEN) #'(b-if COND THEN #f)]
  [(_ COND THEN ELSE) #'(let ([result (if (not (zero? COND))
                                          THEN
                                          ELSE)])
                          (when (exact-positive-integer? result)
                            (b-goto result)))])

(define bool-int (λ (val) (if val 1 0)))
(define bi= (compose1 bool-int =))
(define bi< (compose1 bool-int <))
(define bi> (compose1 bool-int >))

;; b-comp-expr : b-cond-expr [("and" | "or") b-cond-expr]
(define-macro-cases b-logic-expr
  [(_ ARG) #'ARG]
  [(_ LEFT "and" RIGHT) #'(and LEFT RIGHT)]
  [(_ LEFT "or" RIGHT) #'(or LEFT RIGHT)])

;; b-cond-expr : b-expr [("=" | "<" | ">") b-expr]
(define-macro-cases b-comp-expr
  [(_ ARG) #'ARG]
  [(_ LEFT "=" RIGHT) #'(bi= LEFT RIGHT)]
  [(_ LEFT "<" RIGHT) #'(bi< LEFT RIGHT)]
  [(_ LEFT ">" RIGHT) #'(bi> LEFT RIGHT)])

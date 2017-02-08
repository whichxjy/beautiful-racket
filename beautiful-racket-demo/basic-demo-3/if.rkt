#lang br
(require "go.rkt")
(provide b-if b-comp-expr b-logic-expr)

;; b-if : /"if" b-expr /"then" b-expr [/"else" b-expr]
(define (b-if cond-expr then-expr [else-expr #f])
  (cond
    [(not (zero? cond-expr)) (b-goto then-expr)]
    [else-expr => b-goto]))

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

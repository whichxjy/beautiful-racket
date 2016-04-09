#lang br

(module reader br
  (provide read-syntax)
  (require "tokenizer.rkt" "parser.rkt")
  (define (read-syntax src-path src-port)
    (define parsed-syntax (parse src-path (tokenize src-port)))
    ;; `strip-context` because `read-syntax` promises
    ;; a "clean" syntax object without context
    ;; (so later operations can add it)
    (strip-context
     (inject-syntax ([parsed-syntax])
                    #'(module bf-interpreter br-bf
                        parsed-syntax)))))

(provide (rename-out [bf-module-begin #%module-begin])
         #%top-interaction bf-program op loop)

;; just relying on br's #%module-begin.
;; Could just as easily pass through that one.
(define #'bf-module-begin #'#%module-begin) 


;; macros to expand our parse tree into local functions

;; bf-program doesn't do anything
(define #'(bf-program <op-or-loop> ...)
  #'(begin <op-or-loop> ...))

;; op branches. Note that string & number literals are
;; matched literally in syntax patterns.
(define-cases #'op
  [#'(_ ">") #'(move-pointer 1)]
  [#'(_ "<") #'(move-pointer -1)]
  [#'(_ "+") #'(set-pointer-byte! (add1 (get-pointer-byte)))]
  [#'(_ "-") #'(set-pointer-byte! (sub1 (get-pointer-byte)))]
  [#'(_ ".") #'(write-byte (get-pointer-byte))]
  [#'(_ ",") #'(set-pointer-byte! (read-byte))])


(define #'(loop "[" <op-or-loop> ... "]")
  #'(until (zero? (get-pointer-byte))
           <op-or-loop> ...))

;; bf implementation

;; state: one vector, one pointer
(define bf-vector (make-vector 30000 0))
(define bf-pointer 0)

;; gets and sets
(define (get-pointer-byte) (vector-ref bf-vector bf-pointer))
(define (set-pointer-byte! val) (vector-set! bf-vector bf-pointer val))

;; pointer mover
(define (move-pointer how-far) (set! bf-pointer (+ bf-pointer how-far)))
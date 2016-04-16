#lang br
(provide (all-defined-out)
         #%top-interaction
         #%datum
         (rename-out [basic-module-begin #%module-begin]))

(define #'(basic-module-begin PARSE-TREE ...)
  #'(#%module-begin
     'PARSE-TREE ...))

(define #'(basic-program LINE ...)
  #'(begin
      (define program-lines (vector LINE ...))
      (run program-lines)))

(define (run program-lines)
  (for/fold ([line-idx 0])
            ([i (in-naturals)]
             #:break (= line-idx (vector-length program-lines)))
    (match-define (list line-number proc jump-number)
      (vector-ref program-lines line-idx))
    (when proc (proc))
    (if jump-number
        (for/first ([idx (in-range (vector-length program-lines))]
                    #:when (= (car (vector-ref program-lines idx)) jump-number))
                   idx)
        (add1 line-idx))))

;; model each line as (list line-number line-thunk jump)
;; if jump is #f, that means go to the next line
;; a `GOTO` would not have a line-thunk, just a jump
;; what about `GOSUB`? A jump with a return jump ...
(define-cases #'line
  [#'(line 'end) #'(list #f #f #f)]
  [#'(_ NUMBER (statement ARG ...) 'end) #'(list NUMBER (statement ARG ...) #f)]
  [#'(_ (statement ARG ...) 'end) #'(list #f (statement ARG ...) #f)])

(define-cases #'statement
  [#'(_ "PRINT" EXPR-LIST) #'(λ _ (begin (for-each display EXPR-LIST) (displayln "")))])

(define-cases #'expr-list
  [#'(_ EXPR ...) #'(list EXPR ...)])


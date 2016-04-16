#lang br
(provide (all-defined-out)
         #%top-interaction
         #%datum
         (rename-out [basic-module-begin #%module-begin]))

(define #'(basic-module-begin PARSE-TREE ...)
  #'(#%module-begin
     PARSE-TREE ...))

(define #'(basic-program LINE ...)
  #'(basic-run LINE ...))

(define (basic-run . lines)
  (define program-lines (list->vector (filter (λ(x) x) lines)))
  (for/fold ([line-idx 0])
            ([i (in-naturals)]
             #:break (= line-idx (vector-length program-lines)))
    (match-define (cons line-number proc)
      (vector-ref program-lines line-idx))
    (define maybe-jump-number (and proc (proc)))
    (if (number? maybe-jump-number)
        (let ([jump-number maybe-jump-number])
          (for/or ([idx (in-range (vector-length program-lines))])
                  (and (= (car (vector-ref program-lines idx)) jump-number)
                       idx)))
        (add1 line-idx))))

;; model each line as (cons line-number line-thunk)
(define-cases #'line
  [#'(line 'end) #'#f]
  [#'(_ NUMBER STATEMENT 'end) #'(cons NUMBER (λ _ STATEMENT))]
  [#'(_ STATEMENT 'end) #'(cons #f (λ _ STATEMENT))])

(define #'(statement NAME ARG ...) #'(NAME ARG ...))

(define #'(expression ITEM) #'ITEM)
(define #'(unsignedexpr ITEM) #'ITEM)
(define #'(term ITEM) #'ITEM)
(define #'(factor ITEM) #'ITEM)
(define #'(number ITEM) #'ITEM)

(define #'(printitem EXPR-OR-STRING) #'EXPR-OR-STRING)

(define #'(printlist ITEM-OR-SEPARATOR ...) #'(list ITEM-OR-SEPARATOR ...))

(define (PRINT args)
  (for-each display args)
  (displayln ""))

(define (GOTO where)
  where)


(define-cases #'expr-list
  [#'(_ EXPR ...) #'(list EXPR ...)])


#lang br
(provide (all-defined-out)
         #%top-interaction
         #%datum
         (rename-out [basic-module-begin #%module-begin]))
(require (for-syntax racket/string))

(define #'(basic-module-begin PARSE-TREE ...)
  #'(#%module-begin
     (println (quote PARSE-TREE ...))
     'PARSE-TREE ...))

(define #'(basic-program LINE ...)
  #'(basic-run LINE ...))

(define (basic-run . lines)
  (define program-lines (list->vector (filter (λ(x) x) lines)))
  (void (for/fold ([line-idx 0])
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
              (add1 line-idx)))))

(define #'(CR) #'#f)

(define #'(REM ARG ...) #'(void (list 'ARG ...)))

;; model each line as (cons line-number line-thunk)
(define-cases #'line
  [#'(_ NUMBER . SEPARATED-STMTS)
   #`(cons NUMBER
           (λ _ (begin
                  #,@(for/list ([(item idx) (in-indexed (syntax->list #'SEPARATED-STMTS))]
                                #:when (even? idx))
                               item))))]
  [#'(_ ARG ...) #'(line #f ARG ...)])

(define #'(statement NAME ARG ...) #'(NAME ARG ...))

(define #'(expression ITEM) #'ITEM)
(define #'(unsignedexpr ITEM) #'ITEM)
(define #'(term ITEM) #'ITEM)
(define #'(factor ITEM) #'ITEM)
(define #'(number ITEM) #'ITEM)
(define #'(varlist ITEM) #'ITEM)
(define #'(var ITEM) #'ITEM)


(define #'(printitem EXPR-OR-STRING) #'EXPR-OR-STRING)

;; skip separators
(define #'(printlist . SEPARATED-ITEMS) #`(list #,@(for/list ([(item idx) (in-indexed (syntax->list #'SEPARATED-ITEMS))]
                                                              #:when (even? idx))
                                                             item)))

(define #'(separator SEP) #'(void))

(define #'(function NAME EXP ")") #`(#,(string->symbol (string-trim (syntax->datum #'NAME) "(")) EXP))

(define (TAB expr)
  (make-string expr #\space))

(define (PRINT . args)
  (println args)
  (if (and (= (length args) 1) (list? (car args)))
      (begin
        (for-each display (car args))
        (displayln ""))
      (filter (λ(i) (and (equal? i ":") (displayln ""))) args)))

(define (GOTO where)
  where)

(define vars (make-hasheq))
(define (INPUT id)
  (hash-set! vars (string->symbol id) (read (open-input-string (read-line)))))

(define-cases #'expr-list
  [#'(_ EXPR ...) #'(list EXPR ...)])


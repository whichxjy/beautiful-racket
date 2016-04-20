#lang br
(provide #%top-interaction #%app #%datum
         (rename-out [basic-module-begin #%module-begin])
         (rename-out [basic-top #%top])
         (all-defined-out))
(require (for-syntax racket/syntax racket/list))

(define-syntax (basic-module-begin stx)
  (syntax-case stx ()
    [(_ PARSE-TREE ...)
     (with-syntax ([(VARNAME ...) (map (λ(c) (format-id stx "~a" (integer->char c))) (range 65 91))]) 
       #'(#%module-begin
          (define VARNAME 0) ...
          (println (quote PARSE-TREE ...))
          PARSE-TREE ...))]))

; #%app and #%datum have to be present to make #%top work
(define #'(basic-top . id)
  #'(begin
      (displayln (format "got unbound identifier: ~a" 'id))
      (procedure-rename (λ xs (cons 'id xs)) (format-datum "undefined:~a" 'id))))

(define #'(program LINE ...)
  #'(run (list LINE ...)))

(define (run lines)
  (define program-lines (list->vector (filter (λ(ln) (not (equal? ln "cr"))) lines)))
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

(define-cases #'cr-line
  [#'(_ ARG ...) #'(begin ARG ...)])

(define #'(line NUMBER STATEMENT ...)
  #'(cons NUMBER (λ _ STATEMENT ...)))


(define vars (make-hasheq))

(define-cases #'statement
  [#'(statement ID "=" EXPR) #'(set! ID EXPR)]
  [#'(statement PROC ARG ...) #'(PROC ARG ...)])

(define-cases #'value
  [#'(value "(" EXPR ")") #'EXPR]
  [#'(value ID "(" ARG ... ")") #'(ID ARG ...)]
  [#'(value ID-OR-DATUM) #'ID-OR-DATUM])

(define-cases expr
  [(_ lexpr op rexpr) (if (op lexpr rexpr)
                          1
                          0)]
  [(_ expr) expr])
(provide < > <= >=)

(define-cases sum
  [(_ term op sum) (op term sum)]
  [(_ term) term])
(provide - +)

(define-cases product
  [(_ factor op product) (op factor product)]
  [(_ factor) factor])
(provide * /)

(define print-list list)

(define (PRINT args)
  (match args
    [(list) (displayln "")]
    [(list items ... ";" pl) (begin (for-each display items) (PRINT pl))]
    [(list items ... ";") (for-each display items)]
    [(list items ...) (for-each displayln items)]))

(define (TAB num) (make-string num #\space))
(define (INT num) (inexact->exact (round num)))
(define (SIN num) (sin num))
(define (RND num) (* (random) num))

(define (GOTO where)
  where)

(define (comment . args) void)
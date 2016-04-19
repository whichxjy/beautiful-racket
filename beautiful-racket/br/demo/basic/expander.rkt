#lang br
(provide #%top-interaction #%app #%datum
         (rename-out [basic-module-begin #%module-begin])
         (rename-out [basic-top #%top])
         (all-defined-out))
(require (for-syntax racket/syntax))

(define #'(basic-module-begin PARSE-TREE ...)
  #'(#%module-begin
     (println (quote PARSE-TREE ...))
     PARSE-TREE ...))

; #%app and #%datum have to be present to make #%top work
(define #'(basic-top . id)
  #'(begin
      (displayln (format "got unbound identifier: ~a" 'id))
      (procedure-rename (λ xs (cons 'id xs)) (format-datum "undefined:~a" 'id))))

(define #'(basic-program CR-LINE ...)
  #'(basic-run CR-LINE ...))

(define (basic-run . lines)
  (define program-lines (list->vector lines))
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

(define #'(cr-line "cr" ARG ...)
  #'(begin ARG ...))

(define #'(line NUMBER STATEMENT ...)
  #'(cons NUMBER (λ _ STATEMENT ...)))


(define vars (make-hasheq))

(define-cases #'statement
  [#'(statement ID "=" EXPR) #'(hash-set! vars 'ID EXPR)]
  [#'(statement PROC ARG ...) #'(PROC ARG ...)])

(define-cases #'value
  [#'(value "(" EXPR ")") #'EXPR]
  [#'(value ID "(" ARG ... ")") #'(ID ARG ...)]
  [#'(value ID-OR-DATUM) #'(hash-ref vars 'ID-OR-DATUM (λ _ ID-OR-DATUM))])

(define #'(expr EXPR) #'EXPR)

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

(define (GOTO where)
  where)

(define (comment . args) void)

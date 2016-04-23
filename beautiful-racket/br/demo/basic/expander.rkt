#lang br
(provide #%top-interaction #%app #%datum
         (rename-out [basic-module-begin #%module-begin])
         (rename-out [basic-top #%top])
         (all-defined-out))
(require br/stxparam (for-syntax br/datum))

; BASIC implementation details
; http://www.atariarchives.org/basicgames/showpage.php?page=i12

(define-language-variables [A 0][B 0][C 0][D 0][E 0][F 0][G 0][H 0][I 0][J 0][K 0][L 0][M 0][N 0][O 0][P 0][Q 0][R 0][S 0][T 0][U 0][V 0][W 0][X 0][Y 0][Z 0][A$ ""][B$ ""][C$ ""][D$ ""][E$ ""][F$ ""][G$ ""][H$ ""][I$ ""][J$ ""][K$ ""][L$ ""][M$ ""][N$ ""][O$ ""][P$ ""][Q$ ""][R$ ""][S$ ""][T$ ""][U$ ""][V$ ""][W$ ""][X$ ""][Y$ ""][Z$ ""])

(define #'(basic-module-begin _parse-tree ...)
  #'(#%module-begin
     (inject-language-variables (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A$ B$ C$ D$ E$ F$ G$ H$ I$ J$ K$ L$ M$ N$ O$ P$ Q$ R$ S$ T$ U$ V$ W$ X$ Y$ Z$)
                                (println (quote _parse-tree ...))
                                _parse-tree ...)))

; #%app and #%datum have to be present to make #%top work
(define #'(basic-top . id)
  #'(begin
      (displayln (format "got unbound identifier: ~a" 'id))
      (procedure-rename (λ xs (cons 'id xs)) (string->symbol (format "undefined:~a" 'id)))))

(define #'(program _line ...) #'(run (list _line ...)))


(struct exn:line-not-found exn:fail ())


(define (run lines)
  (define program-lines (list->vector (filter (λ(ln) (not (equal? ln "cr"))) lines)))
  (define (line-number->index ln)
    (or
     (for/or ([idx (in-range (vector-length program-lines))])
             (and (= (car (vector-ref program-lines idx)) ln)
                  idx))
     (raise
      (exn:line-not-found
       (format "line number ~a not found in program" ln)
       (current-continuation-marks)))))
  (for/fold ([program-counter 0])
            ([i (in-naturals)]
             #:break (eq? program-counter 'end))
    (cond
      [(= program-counter (vector-length program-lines)) (basic:END)]
      [else
       (define line-function (cdr (vector-ref program-lines program-counter)))
       (define maybe-next-line (and line-function (line-function)))
       (cond
         [(number? maybe-next-line) (line-number->index maybe-next-line)]
         [(eq? 'end maybe-next-line) 'end]
         [else (add1 program-counter)])]))
  (void))

(define #'(cr-line _arg ...) #'(begin _arg ...))


(define current-return-stack (make-parameter empty))

(define-cases #'line
  [#'(_ _NUMBER (statement-list (statement "GOSUB" _WHERE)))
   #'(cons _NUMBER
           (λ _
             (let ([return-stack (current-return-stack)])
               (cond
                 [(or (empty? return-stack)
                      (not (= _NUMBER (car return-stack))))
                  (current-return-stack (cons _NUMBER (current-return-stack)))
                  (basic:GOTO _WHERE)]
                 [else (current-return-stack (cdr (current-return-stack)))]))))]
  [#'(_ _NUMBER _STATEMENT-LIST) #'(cons _NUMBER (λ _ _STATEMENT-LIST))])

(define-cases #'statement-list
  [#'(_ _STATEMENT) #'(begin _STATEMENT)]
  [#'(_ _STATEMENT ":" _STATEMENT-LIST) #'(begin _STATEMENT _STATEMENT-LIST)])

(define-cases #'statement
  [#'(statement _ID "=" _EXPR) #'(set! _ID _EXPR)]
  ;[#'(statement "PRINT" ARG ...) #'(print ARG ...)]
  ;[#'(statement "RETURN" ARG ...) #'(return ARG ...)]
  ;[#'(statement "END" ARG ...) #'(end ARG ...)]
  [#'(statement _proc-string _arg ...)
   (inject-syntax ([#'PROC-ID (format-datum "basic:~a" #'_proc-string)])
                  #'(PROC-ID _arg ...))])

(define-cases #'basic:IF
  [#'(_ _COND "THEN" _TRUE-RESULT "ELSE" _FALSE-RESULT)
   #'(if (true? _COND)
         _TRUE-RESULT
         _FALSE-RESULT)]
  [#'(_ _COND "THEN" _TRUE-RESULT)
   #'(when (true? _COND)
       _TRUE-RESULT)])

(define-cases #'value
  [#'(value "(" _EXPR ")") #'_EXPR]
  [#'(value _ID "(" _ARG ... ")") #'(_ID _ARG ...)]
  [#'(value _ID-OR-DATUM) #'_ID-OR-DATUM])

(define true? (compose1 not zero?))
(define (cond->int cond) (if cond 1 0))
(define (basic:and . args) (cond->int (andmap true? args)))
(define (basic:or . args) (cond->int (ormap true? args)))

(define-cases #'expr-list
  [#'(_ _EXPR) #'_EXPR]
  [#'(_ _EXPR "," _EXPR-LIST) #'(_EXPR _EXPR-LIST)])

(define-cases #'expr
  [#'(_ _COMP-EXPR "AND" _SUBEXPR) #'(basic:and _COMP-EXPR _SUBEXPR)]
  [#'(_ _COMP-EXPR "OR" _SUBEXPR) #'(basic:or _COMP-EXPR _SUBEXPR)]
  [#'(_ _COMP-EXPR) #'_COMP-EXPR])

(define-cases #'comp-expr
  [#'(_ _LEXPR "=" _REXPR) #'(comp-expr _LEXPR "equal?" _REXPR)] ; special case because = is overloaded
  [#'(_ _LEXPR _op _REXPR) (inject-syntax ([#'OP (string->symbol (syntax->datum #'_op))])
                                          #'(cond->int (OP _LEXPR _REXPR)))]
  [#'(_ _ARG) #'_ARG])
(define <> (compose1 not equal?))

(define-cases #'sum
  [#'(_ _TERM "+" _SUM) #'(+ _TERM _SUM)]
  [#'(_ _TERM "-" _SUM) #'(- _TERM _SUM)]
  [#'(_ _TERM) #'_TERM])

(define-cases #'product
  [#'(_ _value "*" _product) #'(* _value _product)]
  [#'(_ _value "/" _product) #'(/ _value _product)]
  [#'(_ _value) #'_value])

(define print-list list)

(define (basic:PRINT args)
  (match args
    [(list) (displayln "")]
    [(list print-list-item ... ";" pl) (begin (for-each display print-list-item)
                                              (basic:PRINT pl))]
    [(list print-list-item ... ";") (for-each display print-list-item)]
    [(list print-list-item ...) (for-each displayln print-list-item)]))

(define (TAB num) (make-string num #\space))
(define #'(INT _ARG ...) #'(inexact->exact (truncate (expr _ARG ...))))
(define (SIN num) (sin num))
(define (ABS num) (inexact->exact (abs num)))
(define (RND num) (* (random) num))

(define-cases #'basic:INPUT
  [#'(_ _PRINT-LIST ";" _ID)
   #'(begin
       (basic:PRINT (append _PRINT-LIST (list ";")))
       (basic:INPUT _ID))]
  [#'(_ _ID) #'(set! _ID (let* ([str (read-line)]
                                [num (string->number str)])
                           (if num num str)))])

(define (basic:GOTO where) where)

(define (basic:RETURN) (car (current-return-stack)))

(define (basic:END)
  'end)

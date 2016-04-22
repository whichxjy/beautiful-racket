#lang br
(provide #%top-interaction #%app #%datum
         (rename-out [basic-module-begin #%module-begin])
         (rename-out [basic-top #%top])
         (all-defined-out))
(require br/stxparam)

(define-language-variables [A 0][B 0][C 0][D 0][E 0][F 0][G 0][H 0][I 0][J 0][K 0][L 0][M 0][N 0][O 0][P 0][Q 0][R 0][S 0][T 0][U 0][V 0][W 0][X 0][Y 0][Z 0][A$ ""][B$ ""][C$ ""][D$ ""][E$ ""][F$ ""][G$ ""][H$ ""][I$ ""][J$ ""][K$ ""][L$ ""][M$ ""][N$ ""][O$ ""][P$ ""][Q$ ""][R$ ""][S$ ""][T$ ""][U$ ""][V$ ""][W$ ""][X$ ""][Y$ ""][Z$ ""])

(define #'(basic-module-begin PARSE-TREE ...)
  #'(#%module-begin
     (inject-language-variables (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A$ B$ C$ D$ E$ F$ G$ H$ I$ J$ K$ L$ M$ N$ O$ P$ Q$ R$ S$ T$ U$ V$ W$ X$ Y$ Z$)
                                (println (quote PARSE-TREE ...))
                                PARSE-TREE ...)))

; #%app and #%datum have to be present to make #%top work
(define #'(basic-top . id)
  #'(begin
      (displayln (format "got unbound identifier: ~a" 'id))
      (procedure-rename (λ xs (cons 'id xs)) (string->symbol (format "undefined:~a" 'id)))))

(define #'(program LINE ...) #'(run (list LINE ...)))

(define (run lines)
  (define program-lines (list->vector (filter (λ(ln) (not (equal? ln "cr"))) lines)))
  (void (with-handlers ([exn:program-end? (λ (exn) (void))])
          (for/fold ([program-counter 0])
                    ([i (in-naturals)]
                     #:break (= program-counter (vector-length program-lines)))
            (match-define (cons line-number proc)
              (vector-ref program-lines program-counter))
            (define maybe-jump-number (and proc (proc)))
            (if (number? maybe-jump-number)
                (let ([jump-number maybe-jump-number])
                  (for/or ([idx (in-range (vector-length program-lines))])
                          (and (= (car (vector-ref program-lines idx)) jump-number)
                               idx)))
                (add1 program-counter))))))

(define #'(cr-line ARG ...) #'(begin ARG ...))


(define current-return-stack (make-parameter empty))

(define-cases #'line
  [#'(_ NUMBER (STATEMENT "GOSUB" WHERE)) #'(cons NUMBER
                                                  (λ _
                                                    (current-return-stack (cons NUMBER (current-return-stack)))
                                                    (GOTO WHERE)))]
  [#'(_ NUMBER STATEMENT ...)  #'(cons NUMBER (λ _ STATEMENT ...))])


(define-cases #'statement
  [#'(statement ID "=" EXPR) #'(set! ID EXPR)]
  [#'(statement PROC ARG ...) #'(PROC ARG ...)])

(define-cases #'IF
  [#'(_ COND "THEN" TRUE-RESULT "ELSE" FALSE-RESULT)
   #'(if (true? COND)
         TRUE-RESULT
         FALSE-RESULT)]
  [#'(_ COND "THEN" TRUE-RESULT)
   #'(when (true? COND)
       TRUE-RESULT)])

(define-cases #'value
  [#'(value "(" EXPR ")") #'EXPR]
  [#'(value ID "(" ARG ... ")") #'(ID ARG ...)]
  [#'(value ID-OR-DATUM) #'ID-OR-DATUM])

(define true? (compose1 not zero?))

(define-cases #'expr
  [#'(_ LEXPR "AND" REXPR)
   #'(if (and (true? LEXPR) (true? REXPR)) 1 0)]
  [#'(_ LEXPR "OR" REXPR)
   #'(if (or (true? LEXPR) (true? REXPR)) 1 0)]
  [#'(_ EXPR) #'EXPR])

(define-cases #'comp-expr
  [#'(_ lexpr "=" rexpr) #'(comp-expr lexpr equal? rexpr)] ; special case because = is overloaded
  [#'(_ lexpr op rexpr) #'(if (op lexpr rexpr) 1 0)]
  [#'(_ expr) #'expr])
(define (<> lexpr rexpr) (not (equal? lexpr rexpr)))
(provide < > <= >= <>)

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
    [(list print-list-item ... ";" pl) (begin (for-each display print-list-item)
                                              (display " ")
                                              (PRINT pl))]
    [(list print-list-item ... ";") (begin
                                      (for-each display print-list-item)
                                      (display " "))]
    [(list print-list-item ...) (for-each displayln print-list-item)]))

(define (TAB num) (make-string num #\space))
(define #'(INT EXPR ...) #'(inexact->exact (round (expr EXPR ...))))
(define (SIN num) (sin num))
(define (ABS num) (inexact->exact (abs num)))
(define (RND num) (* (random) num))

(define-cases #'INPUT
  [#'(_ PRINT-LIST ";" ID)
   #'(begin
       (PRINT (append PRINT-LIST (list ";")))
       (INPUT ID))]
  [#'(_ ID) #'(set! ID (let* ([str (read-line)]
                              [num (string->number str)])
                         (if num num str)))])

(define (GOTO where)
  where)

(define (GOSUB where)
  where)

(define (RETURN)
  (define where (car (current-return-stack)))
  (current-return-stack (cdr (current-return-stack)))
  where)


(struct exn:program-end exn:fail ())
(define (END)
  (raise
   (exn:program-end
    "program ended"
    (current-continuation-marks))))

(define (comment . args) void)

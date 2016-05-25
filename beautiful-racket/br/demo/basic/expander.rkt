#lang br
(provide #%top-interaction #%app #%datum
         (rename-out [basic-module-begin #%module-begin])
         (rename-out [basic-top #%top])
         (all-defined-out))
(require br/stxparam (for-syntax br/datum))

; BASIC implementation details
; http://www.atariarchives.org/basicgames/showpage.php?page=i12

(define-language-variables [A 0][B 0][C 0][D 0][E 0][F 0][G 0][H 0][I 0][J 0][K 0][L 0][M 0][N 0][O 0][P 0][Q 0][R 0][S 0][T 0][U 0][V 0][W 0][X 0][Y 0][Z 0][A$ ""][B$ ""][C$ ""][D$ ""][E$ ""][F$ ""][G$ ""][H$ ""][I$ ""][J$ ""][K$ ""][L$ ""][M$ ""][N$ ""][O$ ""][P$ ""][Q$ ""][R$ ""][S$ ""][T$ ""][U$ ""][V$ ""][W$ ""][X$ ""][Y$ ""][Z$ ""])

(define-macro (basic-module-begin SRC-EXPR ...)
  #'(#%module-begin
     (inject-language-variables (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z A$ B$ C$ D$ E$ F$ G$ H$ I$ J$ K$ L$ M$ N$ O$ P$ Q$ R$ S$ T$ U$ V$ W$ X$ Y$ Z$)
                                (println (quote SRC-EXPR ...))
                                SRC-EXPR ...)))

; #%app and #%datum have to be present to make #%top work
(define-macro (basic-top . ID)
  #'(begin
      (displayln (format "got unbound identifier: ~a" 'ID))
      (procedure-rename (λ xs (cons 'ID xs)) (string->symbol (format "undefined:~a" 'ID)))))

(define-macro (program LINE ...) #'(run (list LINE ...)))


(struct exn:line-not-found exn:fail ())
(struct exn:program-end exn:fail ())


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
  (with-handlers ([exn:program-end? (λ(exn) (void))])
    (void
     (for/fold ([program-counter 0])
               ([i (in-naturals)])
       (cond
         [(= program-counter (vector-length program-lines)) (basic:END)]
         [else
          (define line-function (cdr (vector-ref program-lines program-counter)))
          (define maybe-next-line (and line-function (line-function)))
          (cond
            [(number? maybe-next-line) (line-number->index maybe-next-line)]
            [else (add1 program-counter)])])))))

(define-macro (cr-line ARG ...) #'(begin ARG ...))


(define current-return-stack (make-parameter empty))

(define-macro line
  [(_ NUMBER (statement-list (statement "GOSUB" WHERE)))
   #'(cons NUMBER
           (λ _
             (let ([return-stack (current-return-stack)])
               (cond
                 [(or (empty? return-stack)
                      (not (= NUMBER (car return-stack))))
                  (current-return-stack (cons NUMBER (current-return-stack)))
                  (basic:GOTO WHERE)]
                 [else (current-return-stack (cdr (current-return-stack)))]))))]
  [(_ NUMBER STATEMENT ...) #'(cons NUMBER (λ _ STATEMENT ...))])


(define-macro statement
  [(statement ID "=" EXPR) #'(set! ID EXPR)]
  [(statement PROC-STRING ARG ...)
   (with-pattern
    ([PROC-ID (prefix-id "basic:" #'PROC-STRING)])
    #'(PROC-ID ARG ...))])

(define-macro basic:IF
  [(_ COND "THEN" TRUE-RESULT "ELSE" FALSE-RESULT)
   #'(if (true? COND)
         TRUE-RESULT
         FALSE-RESULT)]
  [(_ COND "THEN" TRUE-RESULT)
   #'(when (true? COND)
       TRUE-RESULT)])

(define true? (compose1 not zero?))
(define (cond->int cond) (if cond 1 0))
(define (basic:and . args) (cond->int (andmap true? args)))
(define (basic:or . args) (cond->int (ormap true? args)))

(define-macro expr
  [(_ COMP-EXPR "AND" SUBEXPR) #'(basic:and COMP-EXPR SUBEXPR)]
  [(_ COMP-EXPR "OR" SUBEXPR) #'(basic:or COMP-EXPR SUBEXPR)]
  [(_ COMP-EXPR) #'COMP-EXPR])

(define-macro comp-expr
  [(_ LEXPR "=" REXPR) #'(comp-expr LEXPR "equal?" REXPR)] ; special case because = is overloaded
  [(_ LEXPR OP-STR REXPR) (with-pattern ([OP (replace-context #'here (prefix-id #'OP-STR))])
                                        #'(cond->int (OP LEXPR REXPR)))]
  [(_ ARG) #'ARG])

(define <> (compose1 not equal?))

(define-macro sum
  [(_ TERM "+" SUM) #'(+ TERM SUM)]
  [(_ TERM "-" SUM) #'(- TERM SUM)]
  [(_ TERM) #'TERM])

(define-macro product
  [(_ VALUE "*" PRODUCT) #'(* VALUE PRODUCT)]
  [(_ VALUE "/" PRODUCT) #'(/ VALUE PRODUCT)]
  [(_ VALUE) #'VALUE])

(define print-list list)

(define (basic:PRINT args)
  (match args
    [(list) (displayln "")]
    [(list print-list-item ... ";" pl) (begin (for-each display print-list-item)
                                              (basic:PRINT pl))]
    [(list print-list-item ... ";") (for-each display print-list-item)]
    [(list print-list-item ...) (for-each displayln print-list-item)]))

(define (TAB num) (make-string num #\space))
(define-macro (INT _ARG ...) #'(inexact->exact (truncate (expr _ARG ...))))
(define (SIN num) (sin num))
(define (ABS num) (inexact->exact (abs num)))
(define (RND num) (* (random) num))

(define-macro basic:INPUT
  [(_ PRINT-LIST ";" _ID)
   #'(begin
       (basic:PRINT (append PRINT-LIST (list ";")))
       (basic:INPUT _ID))]
  [(_ ID) #'(set! ID (let* ([str (read-line)]
                            [num (string->number str)])
                       (or num str)))])

(define (basic:GOTO where) where)

(define (basic:RETURN) (car (current-return-stack)))

(define (basic:END)
  (raise
   (exn:program-end
    ""
    (current-continuation-marks))))

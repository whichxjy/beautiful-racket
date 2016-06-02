#lang br
(provide #%top-interaction #%app #%datum
         (rename-out [basic-module-begin #%module-begin])
         (rename-out [basic-top #%top])
         (all-defined-out))

; BASIC implementation details
; http://www.atariarchives.org/basicgames/showpage.php?page=i12

(begin-for-syntax
  (require racket/list)
  (define (gather-unique-ids stx)
    (remove-duplicates (map syntax->datum (filter (λ(s) (syntax-property s 'id)) (syntax-flatten stx))) eq?)))

(define-macro (basic-module-begin (basic-program PROGRAM-LINE ...))
  (with-pattern
   ([(UNIQUE-ID ...) (map (compose1 syntax-local-introduce (λ(id) (datum->syntax #f id)))
                          (gather-unique-ids #'(PROGRAM-LINE ...)))])
   #'(#%module-begin
      (define UNIQUE-ID 0) ...
      (run PROGRAM-LINE ...))))

; #%app and #%datum have to be present to make #%top work
(define-macro (basic-top . ID)
  #'(begin
      (displayln (format "got unbound identifier: ~a" 'ID))
      (procedure-rename (λ xs (cons 'ID xs)) (string->symbol (format "undefined:~a" 'ID)))))

(struct exn:line-not-found exn:fail ())
(define (raise-line-not-found-error ln)
  (raise
   (exn:line-not-found
    (format "line number ~a not found in program" ln)
    (current-continuation-marks))))

(struct end-program-signal exn:fail ())
(define (raise-end-program-signal)
  (raise (end-program-signal "" (current-continuation-marks))))

(struct end-line-signal exn:fail ())
(define (raise-end-line-signal)
  (raise (end-line-signal "" (current-continuation-marks))))

(define (run . line-list)
  (define lines (list->vector line-list))
  (define (find-index ln)
    (or
     (for/or ([idx (in-range (vector-length lines))])
             (and (= ($line-number (vector-ref lines idx)) ln)
                  idx))
     (raise-line-not-found-error ln)))
  (void
   (with-handlers ([end-program-signal? void])
     (for/fold ([program-counter 0])
               ([i (in-naturals)])
       (if (= program-counter (vector-length lines))
           (basic:end)
           (let* ([line-thunk ($line-thunk (vector-ref lines program-counter))]
                  [maybe-line-number (line-thunk)])
             (if (number? maybe-line-number)
                 (find-index maybe-line-number)
                 (add1 program-counter))))))))

(define return-stack empty)

(define (do-gosub this-line where)
  (if (or (empty? return-stack)
          (not (= this-line (car return-stack))))
      (begin
        (set! return-stack (cons this-line return-stack))
        (basic:goto where))
      ;; if (= number (car return-stack))
      ;; then we reached this line by `return`, which means the end of a gosub
      (set! return-stack (cdr return-stack))))

(struct $line (number thunk) #:transparent)
(define-macro line
  [(_ NUMBER (statement "gosub" WHERE))
   #'($line NUMBER (λ () (do-gosub NUMBER WHERE)))]
  [(_ NUMBER . STATEMENTS)
   #'($line NUMBER (λ () (with-handlers ([end-line-signal? (λ _ #f)])
                           . STATEMENTS)))])

(define-macro statement
  [(statement ID "=" EXPR) #'(set! ID EXPR)]
  [(statement PROC-NAME . ARGS)
   (with-pattern
    ([PROC-ID (prefix-id "basic:" #'PROC-NAME)])
    #'(PROC-ID . ARGS))])

(define-macro basic:if
  [(_ COND-EXPR TRUE-EXPR FALSE-EXPR)
   #'(if (true? COND-EXPR)
         TRUE-EXPR
         FALSE-EXPR)]
  [(_ COND-EXPR TRUE-EXPR)
   #'(if (true? COND-EXPR)
         TRUE-EXPR
         (raise-end-line-signal))]) ; special short-circuit rule for one-armed conditional

(define true? (compose1 not zero?))
(define (cond->int cond) (if cond 1 0))
(define (basic:and . args) (cond->int (andmap true? args)))
(define (basic:or . args) (cond->int (ormap true? args)))

(define-macro expr
  [(_ COMP-EXPR) #'COMP-EXPR]
  [(_ COMP-EXPR "and" SUBEXPR) #'(basic:and COMP-EXPR SUBEXPR)]
  [(_ COMP-EXPR "or" SUBEXPR) #'(basic:or COMP-EXPR SUBEXPR)])

(define-macro comp-expr
  [(_ SUM) #'SUM]
  [(_ SUM "=" COMP-EXPR)
   #'(cond->int (equal? SUM COMP-EXPR))] ; special case because `=` is overloaded in basic
  [(_ SUM OP-STR COMP-EXPR)
   (with-pattern
    ([OP (replace-context #'here (prefix-id #'OP-STR))])
    #'(cond->int (OP SUM COMP-EXPR)))])

(define <> (compose1 not equal?))

(define-macro sum
  [(_ SUM) #'SUM]
  [(_ SUM "+" PRODUCT) #'(+ SUM PRODUCT)]
  [(_ SUM "-" PRODUCT) #'(- SUM PRODUCT)])

(define-macro product
  [(_ VALUE) #'VALUE]
  [(_ PRODUCT "*" VALUE) #'(* PRODUCT VALUE)]
  [(_ PRODUCT "/" VALUE) #'(/ PRODUCT VALUE)])

(define print-list list)

(define (basic:print [args #f])
  (match args
    [#f (displayln "")]
    [(list print-list-item ... ";" pl) (begin (for-each display print-list-item)
                                              (basic:print pl))]
    [(list print-list-item ... ";") (for-each display print-list-item)]
    [(list print-list-item ...) (for-each displayln print-list-item)]))

(define (TAB num) (make-string num #\space))
(define-macro (INT _ARG ...) #'(inexact->exact (truncate (expr _ARG ...))))
(define (SIN num) (sin num))
(define (ABS num) (inexact->exact (abs num)))
(define (RND num) (* (random) num))

(define-macro basic:input
  [(_ (print-list . PL-ITEMS) ID ...)
   #'(begin
       (basic:print (append (print-list . PL-ITEMS) (list ";")))
       (basic:input ID) ...)]
  [(_ ID ...) #'(begin
                  (set! ID (let* ([str (read-line)]
                                  [num (string->number str)])
                             (or num str))) ...)])

(define (basic:goto where) where)

(define (basic:return) (car return-stack))

(define (basic:stop) (basic:end))
(define (basic:end) (raise-end-program-signal))

(define for-stack empty)

(define (push-for-stack thunk)
  (set! for-stack (cons thunk for-stack)))

(define (pop-for-stack)
  (set! for-stack (cdr for-stack)))

(define-macro basic:for
  [(_ VAR START-VALUE END-VALUE)
   #'(basic:for VAR START-VALUE END-VALUE 1)]
  [(_ VAR START-VALUE END-VALUE STEP-VALUE)
   #'(begin
       (cond
         [(and (pair? for-stack)
               (eq? 'VAR (car (car for-stack))))
          ;; we're already in the midst of a loop, so abort the rest of the line
          (raise-end-line-signal)]
         [else
          ;; initialize the loop counter
          (statement VAR "=" START-VALUE)
          ;; create a point for the `next` statement to return to, using a continuation
          (let/cc return-point-for-next
            (push-for-stack (cons 'VAR
                                  (λ () ; thunk that increments counter & teleports back to beginning of loop
                                    (define next-val (+ VAR STEP-VALUE))
                                    (when (<= next-val END-VALUE)
                                      (set! VAR next-val)
                                      (return-point-for-next))))))
          (raise-end-line-signal)]))])

(define (handle-next [stack-selector-proc car])
  (unless (pair? for-stack)
    (error 'next "for-stack is empty"))
  (let ([for-thunk (cdr (stack-selector-proc for-stack))])
    (unless (for-thunk)
      (pop-for-stack))))

(define-macro basic:next
  [(_ VAR) #'(handle-next (λ(stack) (assq 'VAR stack)))] ; named `next` means find var in stack
  [(_) #'(handle-next)]) ; plain `next` implies var on top of stack

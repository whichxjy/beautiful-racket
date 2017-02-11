#lang br
(require "struct.rkt" "line.rkt" "misc.rkt")
(provide b-end b-goto b-gosub b-return b-for b-next)

(define (b-end) (raise (end-program-signal)))

(define (b-goto num-expr)
  (raise (change-line-signal num-expr)))

(define return-stack empty)

(define (b-gosub num-expr)
  (let/cc return-cc
    (push! return-stack return-cc)
    (b-goto num-expr)))

(define (b-return)
  (unless (pair? return-stack)
    (raise-line-error "return without gosub"))
  (define top-return-k (pop! return-stack))
  (top-return-k))

(define thunk-table (make-hasheq))

(define-macro-cases b-for
  [(_ LOOP-ID START END) #'(b-for LOOP-ID START END 1)]
  [(_ LOOP-ID START END STEP)
   #'(b-let LOOP-ID (let/cc loop-cc
                      (hash-set! thunk-table
                                 'LOOP-ID
                                 (λ ()
                                   (define next-val (+ LOOP-ID STEP))
                                   (if (next-val . in-closed-interval? . START END)
                                       (loop-cc next-val)
                                       (hash-remove! thunk-table 'LOOP-ID))))
                      START))])

(define (in-closed-interval? x start end)
  ((if (< start end) <= >=) start x end))

(define-macro (b-next LOOP-ID)
  #'(begin
      (unless (hash-has-key? thunk-table 'LOOP-ID)
        (raise-line-error "next without for"))
      (define thunk (hash-ref thunk-table 'LOOP-ID))
      (thunk)))
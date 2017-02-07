#lang br
(require "let.rkt" "line.rkt")
(provide b-for b-next)

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
  (if (< start end)
      (<= start x end)
      (<= end x start)))

(define-macro (b-next LOOP-ID)
  #'(begin
      (unless (hash-has-key? thunk-table 'LOOP-ID)
        (raise-line-error "next without for"))
      (define thunk (hash-ref thunk-table 'LOOP-ID))
      (thunk)))
#lang br
(require "structs.rkt" "misc.rkt" "line.rkt")
(provide (matching-identifiers-out #rx"^b-" (all-defined-out)))

(define (b-end) (raise (end-program-signal)))
(define (b-goto num-expr) (raise (change-line-signal num-expr)))

(define gosub-ccs empty)

(define (b-gosub num-expr)
  (let/cc gosub-cc
    (push! gosub-ccs gosub-cc)
    (b-goto num-expr)))

(define (b-return)
  (unless (pair? gosub-ccs)
    (raise (line-error "return without gosub")))
  (define top-return-k (pop! gosub-ccs))
  (top-return-k))

(define (in-closed-interval? x left right)
  (define cmp (if (< left right) <= >=))
  (cmp left x right))

(define-macro-cases b-for
  [(_ ID START END) #'(b-for ID START END 1)]
  [(_ ID START END STEP)
   #'(b-let ID (let/cc top-of-loop-cc
                 (push-thunk!
                  (cons 'ID
                        (λ ()
                          (define next-val (+ ID STEP))
                          (if (next-val . in-closed-interval? . START END)
                              (top-of-loop-cc next-val)
                              (remove-thunk! 'ID)))))
                 START))])

(define for-thunks (make-parameter empty))

(define (push-thunk! thunk)
  (for-thunks (cons thunk (for-thunks))))

(define (remove-thunk! id-sym)
  (for-thunks (remq (assq id-sym (for-thunks)) (for-thunks))))

(define-macro (b-next ID ...) #'(do-next 'ID ...))

(define (do-next [id-sym #f])
  (when (empty? (for-thunks))
    (raise-line-error "next without for"))
  (define for-thunk
    (cdr (if id-sym
             (or (assq id-sym (for-thunks))
                 (raise-line-error "next without for"))
             (car (for-thunks)))))
  (for-thunk))
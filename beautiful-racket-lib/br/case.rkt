#lang racket/base
(require (for-syntax racket/base) br/define)
(provide (all-defined-out))

(define-macro (define-case-macro ID PRED)
  #'(define-syntax (ID stx)
      (syntax-case stx ()
        [(_ test-val
            [(match-vals) . result] (... ...)
            [else . else-result])
         #'(cond
             [(PRED test-val '(match-vals)) . result] (... ...)
             [else . else-result])]
        [(_ test-val
            match-clause (... ...))
         #'(ID test-val
               match-clause (... ...)
               [else (error 'ID "no match")])])))

;; like case but strictly uses `eq?` comparison (as opposed to `equal?`)
(define-case-macro caseq memq)

;; `eqv?` is OK for chars (same as `char=?`)
(define-case-macro casev memv)
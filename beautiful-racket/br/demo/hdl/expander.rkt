#lang br
(provide (all-from-out br) (all-defined-out))
(require "expander0.rkt")


#|

Be your own binding procedure.
Part parser should generate inner `define`.
input-pin-spec parser should generate kw procedure.
chip-name parser should generate outer define/provide form.

(chip Not
      (x)
      (y))

|#

(define-for-syntax (remove-commas stx-expr)
  (filter (λ(i)
            (not (equal? "," (syntax->datum i)))) (syntax->list stx-expr)))

(define #'(pin-spec-out "OUT" _pin-or-comma ... ";")
  (with-syntax ([(_pinid ...) (remove-commas #'(_pin-or-comma ...))])
    #'(begin
        (define _pinid '(1 2 3 4)) ...
        (list _pinid ...))))

(define #'(make-kw-procedure (pin-spec-in "IN" _pin-or-comma ... ";") _pin-spec-out)
  (with-syntax ([(_pinid ...) (remove-commas #'(_pin-or-comma ...))])
    #'(make-keyword-procedure
       (λ (kws kw-args . rest)
         (define kw-pairs (map cons kws kw-args))
         (let ([_pinid (cdr (assq (string->keyword (format "~a" '_pinid)) kw-pairs))] ...)
           
           _pin-spec-out)))))



(define #'(chip-program "CHIP" _topid "{" _pin-spec-in _pin-spec-out "}")
  #`(begin
      (provide _topid)
      (define _topid
        (make-kw-procedure _pin-spec-in _pin-spec-out)
        )
      (require rackunit)
      (check-equal? (_topid #:a 1 #:b 2 #:c 3 #:d 4) '((1 2 3 4)(1 2 3 4)(1 2 3 4)))))
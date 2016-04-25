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

(define #'(make-kw-procedure
           (pin-spec-in "IN" _pinin-or-comma ... ";")
           (pin-spec-out "OUT" _pinout-or-comma ... ";"))
  (inject-syntax ([#'(_pinin ...) (remove-commas #'(_pinin-or-comma ...))]
                [#'(_pinout ...) (remove-commas #'(_pinout-or-comma ...))])
    #'(make-keyword-procedure
       (λ (kws kw-args . rest)
         (define kw-pairs (map cons kws kw-args))
         (let ([_pinin (cdr (assq (string->keyword (format "~a" '_pinin)) kw-pairs))] ...)
           (define _pinout (list _pinin ...)) ...
           (list _pinout ...))))))



(define #'(chip-program "CHIP" _topid "{" _pin-spec-in _pin-spec-out "}")
  #`(begin
      (provide _topid)
      (define _topid
        (make-kw-procedure _pin-spec-in _pin-spec-out)
        )
      (require rackunit)
      (check-equal? (_topid #:a 1 #:b 2 #:c 3 #:d 4) '((1 2 3 4)(1 2 3 4)(1 2 3 4)))))

#|
 PARTS:
             Nand(a=a, b=a, out=x);

|#
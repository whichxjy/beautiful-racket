#lang br
(provide (all-from-out br) chip)

(define #'(chip _Chip
                (_input-pin ...)
                (_output-pin)
                ((_Part [_pin-in _val-id] ... [out _pin-out]) ...))
  #'(begin
      (provide _Chip)
      (define _Chip
        (make-keyword-procedure
         (λ (kws kw-args . rest)
           (define kw-pairs (map cons kws kw-args))
           (let ([_input-pin (cdr (assq (string->keyword (format "~a" '_input-pin)) kw-pairs))] ...)
             (define _pin-out (call-part _Part [_pin-in _val-id] ...)) ...
             _output-pin))))))

(define #'(call-part _Part [_pin-in _val-id] ...)
  (with-syntax ([part-path (format "~a.hdl" (syntax->datum #'_Part))]
                [(kw ...) (map (λ(pi) (string->keyword (format "~a" (syntax->datum pi)))) (syntax->list #'(_pin-in ...)))])
    #'(let ()
        (local-require (rename-in part-path [_Part local-name]))
        (keyword-apply local-name '(kw ...) (list _val-id ...) null))))

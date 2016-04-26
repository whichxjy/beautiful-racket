#lang br
(provide #%top-interaction #%module-begin (all-defined-out))

(define #'(chip-program "CHIP" _arg ...)
  #'(chip _arg ...))

(define #'(pin-spec _label _pin-list ";")
  #'_pin-list)

(define-inverting #'(pin-list _id _another-id ...)
  #'(_id _another-id ...))

(define #'(another-id "," _id)
  #'_id)

(define #'(part-spec "PARTS:" _part-list)
  #'_part-list)

(define #'(part-list _part ...)
  #'(begin _part ...))

(define-inverting #'(part _partname "(" _firstpin "=" _firstval _another-id-pair ... (_lastpin _pinout) ")" ";")
  #'(begin
      (define _pinout (call-part _partname [_firstpin _firstval] _another-id-pair ...))))

(define #'(another-id-pair "," _firstid "=" _secondid)
  #'(_firstid _secondid))

(define #'(call-part _Part [_pin-in _val-id] ...)
  (with-syntax ([part-path (format "~a.hdl" (syntax->datum #'_Part))]
                [(kw ...) (map (λ(pi) (string->keyword (format "~a" (syntax->datum pi)))) (syntax->list #'(_pin-in ...)))])
    #'(let ()
        (local-require (rename-in part-path [_Part local-name]))
        (keyword-apply local-name '(kw ...) (list _val-id ...) null))))

(define-inverting #'(chip _chipname "{"
                          (_input-pin ...)
                          (_output-pin ...)
                          _part-spec "}")
  #'(begin
      (provide _chipname)
      (define _chipname
        (procedure-rename
         (make-keyword-procedure
          (λ (kws kw-args . rest)
            (define kw-pairs (map cons kws kw-args))
            (let ([_input-pin (cdr (assq (string->keyword (format "~a" '_input-pin)) kw-pairs))] ...)
              _part-spec
              (values _output-pin ...)))) '_chipname))))

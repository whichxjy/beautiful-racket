#lang br
(provide #%top-interaction #%module-begin #%app #%datum (all-defined-out))

(define-inverting #'(chip-program "CHIP" _chipname "{"
                                  (_input-pin ...)
                                  (_output-pin ...)
                                  _part-spec "}")
  #'(begin
      (define+provide _chipname
        (procedure-rename
         (make-keyword-procedure
          (λ (kws kw-args . rest)
            (define kw-pairs (map cons kws kw-args))
            (let ([_input-pin (cdr (assq (string->keyword (format "~a" '_input-pin)) kw-pairs))] ...)
              _part-spec
              (values _output-pin ...)))) '_chipname))))

(define-inverting #'(pin-spec _label _pin ... ";")
  #'(_pin ...))

(define-cases #'pin
  [#'(_ _pin ",") #'_pin]
  [#'(_ _pin) #'_pin])

(define #'(part-spec "PARTS:" _part ...)
  #'(begin _part ...))

(define-inverting #'(part _partname "(" (_pin _val) ... (_lastpin _pinout) ")" ";")
  #'(define _pinout (call-part _partname [_pin _val] ...)))

(define-cases #'pin-val-pair
  [#'(_ _pin "=" _val ",") #'(_pin _val)]
  [#'(_ _pin "=" _val) #'(_pin _val)])

(define #'(call-part _partname [_pin _val] ...)
  (inject-syntax ([#'part-path (findf file-exists? (list (format "~a.hdl" (syntax->datum #'_partname)) (format "~a.hdl.rkt" (syntax->datum #'_partname))))]
                  [#'(kw ...) (map (λ(pi) (string->keyword (format "~a" (syntax->datum pi)))) (syntax->list #'(_pin ...)))])
                 #'(let ()
                     (local-require (rename-in part-path [_partname local-name]))
                     (keyword-apply local-name '(kw ...) (list _val ...) null))))

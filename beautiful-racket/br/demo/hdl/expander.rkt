#lang br
(provide #%top-interaction #%module-begin #%app #%datum (all-defined-out))

(define #'(chip-program _chipname
                                  (pin-spec _input-pin ...)
                                  (pin-spec _output-pin ...)
                                  (part-spec (part _partname (_pin _val) ... (_lastpin _pinout)) ...))
  #'(begin
      (define+provide _chipname
        (procedure-rename
         (make-keyword-procedure
          (λ (kws kw-args . rest)
            (define kw-pairs (map cons kws kw-args))
            (let ([_input-pin (cdr (assq (string->keyword (format "~a" '_input-pin)) kw-pairs))] ...)
              (define _pinout (call-part _partname [_pin _val] ...)) ...
              (values _output-pin ...)))) '_chipname))))


(define #'(call-part _partname [_pin _val] ...)
  (inject-syntax ([#'part-path (findf file-exists? (list (format "~a.hdl" (syntax->datum #'_partname)) (format "~a.hdl.rkt" (syntax->datum #'_partname))))]
                  [#'(kw ...) (map (λ(pi) (string->keyword (format "~a" (syntax->datum pi)))) (syntax->list #'(_pin ...)))])
                 #'(let ()
                     (local-require (rename-in part-path [_partname local-name]))
                     (keyword-apply local-name '(kw ...) (list _val ...) null))))

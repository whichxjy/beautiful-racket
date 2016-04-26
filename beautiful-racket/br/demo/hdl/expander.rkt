#lang br
(provide #%top-interaction #%module-begin #%datum #%top #%app)

(provide chip-program)
(define #'(chip-program "CHIP" _arg ...)
  #'(chip _arg ...))

(provide pin-spec-in)
(define #'(pin-spec-in "IN" _pin-list ";")
  #'_pin-list)

(provide pin-spec-out)
(define #'(pin-spec-out "OUT" _pin-list ";")
  #'_pin-list)


(require (for-syntax sugar/debug))


(provide pin-list)
(define #'(pin-list . _pin-or-commas)
  (for/list ([stx (in-list (syntax->list #'_pin-or-commas))]
             #:when (not (equal? "," (report (syntax->datum stx)))))
            stx))

(begin-for-syntax
  (define (expand-macro mac)
    (syntax-disarm (report (local-expand mac 'expression #f)) #f)))

(provide part-spec)
(define #'(part-spec "PARTS:" _part-list)
  #'_part-list)

(provide part-list)
(define #'(part-list _part ";")
  #'_part)

(provide part)
(define #'(part _partname "(" _pin-in "=" _val-id "," _pin-in2 "=" _val-id2 "," out "=" _pin-out ")")
           #'(begin
               (define _pin-out (call-part _partname [_pin-in _val-id][_pin-in2 _val-id2]))))

(define #'(chip _chipname "{"
                _input-pins
                _output-pins
                _part-spec "}")
  (inject-syntax ([#'(_input-pin ...) (expand-macro #'_input-pins)]
                  [#'(_output-pin ...) (expand-macro #'_output-pins)]
                  [#'_part (expand-macro #'_part-spec)])
                 #'(begin
                     (provide _chipname)
                     (define _chipname
                       (make-keyword-procedure
                        (λ (kws kw-args . rest)
                          (define kw-pairs (map cons kws kw-args))
                          (let ([_input-pin (cdr (assq (string->keyword (format "~a" '_input-pin)) kw-pairs))] ...)
                            _part 
                            (values _output-pin ...))))))))

(provide call-part)
(define #'(call-part _Part [_pin-in _val-id] ...)
  (with-syntax ([part-path (format "~a.hdl" (syntax->datum #'_Part))]
                [(kw ...) (map (λ(pi) (string->keyword (format "~a" (syntax->datum pi)))) (syntax->list #'(_pin-in ...)))])
    #'(let ()
        (local-require (rename-in part-path [_Part local-name]))
        (keyword-apply local-name '(kw ...) (list _val-id ...) null))))
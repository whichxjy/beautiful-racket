#lang br
(provide #%top-interaction #%module-begin #%datum #%top #%app)

(provide chip-program)
(define #'(chip-program "CHIP" _arg ...)
  #'(chip _arg ...))

(provide pin-spec)
(define #'(pin-spec _label _pin-list ";")
  #'_pin-list)

(require (for-syntax sugar/debug))


(define-for-syntax (remove-separators stx-or-list sep)
  (for/list ([item (in-list (if (list? stx-or-list)
                                stx-or-list
                                (syntax->list stx-or-list)))]
             #:when (not (equal? sep (syntax->datum item))))
            item))

(provide pin-list)
(define #'(pin-list . _pin-or-commas)
  (remove-separators #'_pin-or-commas ","))

(begin-for-syntax
  (define (expand-macro mac)
    (syntax-disarm (local-expand mac 'expression #f) #f)))

(provide part-spec)
(define #'(part-spec "PARTS:" _part-list)
  #'_part-list)

(provide part-list)
(define #'(part-list . _part-or-semicolons)
  (inject-syntax ([#'(part ...) (remove-separators #'_part-or-semicolons "'")])
                 #'(begin part ...)))

(require (for-syntax sugar/list))
(define-for-syntax (ugly-processing stx)
  (slice-at (remove-separators (remove-separators stx ",") "=") 2))

(provide part)
(define #'(part _partname "(" _pin-id-etc ... out "=" _pin-out ")" ";")
  (with-syntax ([((_pin-in _val-id) ...) (ugly-processing #'(_pin-id-etc ...))])
           #'(begin
               (define _pin-out (call-part _partname [_pin-in _val-id] ...)))))

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
                       (procedure-rename
                        (make-keyword-procedure
                        (λ (kws kw-args . rest)
                          (define kw-pairs (map cons kws kw-args))
                          (let ([_input-pin (cdr (assq (string->keyword (format "~a" '_input-pin)) kw-pairs))] ...)
                            _part 
                            (values _output-pin ...)))) '_chipname)))))

(provide call-part)
(define #'(call-part _Part [_pin-in _val-id] ...)
  (with-syntax ([part-path (format "~a.hdl" (syntax->datum #'_Part))]
                [(kw ...) (map (λ(pi) (string->keyword (format "~a" (syntax->datum pi)))) (syntax->list #'(_pin-in ...)))])
    #'(let ()
        (local-require (rename-in part-path [_Part local-name]))
        (keyword-apply local-name '(kw ...) (list _val-id ...) null))))
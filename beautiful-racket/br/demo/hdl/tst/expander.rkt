#lang br
(provide #%top-interaction #%module-begin #%datum #%top #%app)

(provide tst-program)
(define #'(tst-program _arg ...)
  #'(begin _arg ...))


(define-for-syntax private-proc-name (generate-temporary))

(provide load-expr)
;; parse shape: (load-expr "load" Xor.hdl ",")
(define #'(load-expr "load" _filename ",")
  (inject-syntax ([#'filename-string (symbol->string (syntax->datum #'_filename))]
                  [#'proc-name (string->symbol (cadr (regexp-match #rx"^(.*)\\.hdl$"(symbol->string (syntax->datum #'_filename)))))])
                 #'(begin
                     (define _filename (dynamic-require filename-string 'proc-name)))))

(begin-for-syntax
  (define (expand-macro mac)
    (syntax-disarm (local-expand mac 'expression #f) #f)))

;; parse shape:
;; (header-expr "output-list" a (comma-id "," b)  "," "out" ";")
(provide header-expr)
(define #'(header-expr "output-list" _first-id _comma-id ... "," "out" ";")
  (inject-syntax ([#'(_other-id ...) (map expand-macro (syntax->list #'(_comma-id ...)))])
                 #'(begin
                     (display-header _first-id _other-id ... out)
                     (define _first-id #f)
                     (define _other-id #f) ...
                     (define (out)
                       (keyword-apply proc '(#:a #:b) (list a b) null))
                     )))

(provide comma-id)
(define #'(comma-id "," _id)
  #'_id)


(define #'(display-header _val ...)
  #'(begin
      (apply display-values (list '_val ...))
      (apply display-dashes (list '_val ...))))

(define (vals->text vals)
  (string-join (map ~a vals) " | "))

(define (display-values . vals)
  (displayln (vals->text vals)))

(define (display-dashes . vals)
  (displayln (make-string (string-length (vals->text vals)) #\-)))


(provide test-expr)
(define #'(test-expr _first-step _comma-step ... ";")
  (inject-syntax ([#'(_other-step ...) (expand-macro #'(_comma-step ...))])
                 #'(let ()
                     _first-step
                     _other-step ...)))

(provide step-expr)
(define #'(step-expr _step)
  #'_step)

(provide set-expr)
(define #'(set-expr "set" _id _val)
  #'(set! _id _val))

(provide comma-step)
(define #'(comma-step "," _step)
  #'_step)

(provide eval-expr)
(define #'(eval-expr "eval")
  #'(set! result (param-proc)))

#|
(tst-program (load-expr "load" Xor.hdl ",") (header-expr "output-list" a "," b "," out ";") (test-expr (step-expr (set-expr "set" a 0)) "," (step-expr (set-expr "set" b 0)) "," (step-expr (eval-expr "eval")) "," (step-expr (output-expr "output")) ";") (test-expr (step-expr (set-expr "set" a 0)) "," (step-expr (set-expr "set" b 1)) "," (step-expr (eval-expr "eval")) "," (step-expr (output-expr "output")) ";") (test-expr (step-expr (set-expr "set" a 1)) "," (step-expr (set-expr "set" b 0)) "," (step-expr (eval-expr "eval")) "," (step-expr (output-expr "output")) ";") (test-expr (step-expr (set-expr "set" a 1)) "," (step-expr (set-expr "set" b 1)) "," (step-expr (eval-expr "eval")) "," (step-expr (output-expr "output")) ";"))
|#


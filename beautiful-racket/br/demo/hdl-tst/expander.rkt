#lang br
(provide #%top-interaction #%module-begin #%datum (rename-out [my-top #%top]) #%app
         (all-defined-out))

; #%app and #%datum have to be present to make #%top work
(define #'(my-top . id)
  #'(begin
      (displayln (format "got unbound identifier: ~a" 'id))
      (procedure-rename (λ xs (cons 'id xs)) (string->symbol (format "undefined:~a" 'id)))))

(define #'(tst-program _arg ...) #'(begin _arg ...))


(define #'(header-expr _filename (_colid ... _outid))
  (with-syntax* ([filename-string (symbol->string (syntax->datum #'_filename))]
                 [procname (string->symbol (cadr (regexp-match #rx"^(.*)\\.hdl$"(symbol->string (syntax->datum #'_filename)))))]
                 [output (syntax-local-introduce (datum->syntax #f 'output))])
    #'(begin
        (provide (all-defined-out))
        (define procname
          (dynamic-require (findf file-exists?
                                  (list filename-string (format "~a.rkt" filename-string))) 'procname))
        (display-header '_colid ... '_outid)
        (define _colid (make-parameter 0)) ...
        (define (_outid)
          (keyword-apply procname
                         (map (compose1 string->keyword symbol->string) (list '_colid ...))
                         (list (_colid) ...) null))
        
        (define (output)
          (display-values (_colid) ... (_outid))))))

(define #'(display-header _sym ...)
  #'(begin
      (apply display-values (list _sym ...))
      (apply display-dashes (list _sym ...))))

(define (vals->text vals) (string-join (map ~a vals) " | "))

(define (display-values . vals) (displayln (vals->text vals)))

(define (display-dashes . vals)
  (displayln (make-string (string-length (vals->text vals)) #\-)))

(define #'test-expr #'begin)

(define #'eval-expr #'void)

(define #'(output-expr)
  (inject-syntax ([#'output 'output])
                 #'(output)))

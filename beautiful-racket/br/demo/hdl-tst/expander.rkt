#lang br
(provide #%top-interaction #%module-begin #%datum (rename-out [my-top #%top]) #%app
         (all-defined-out))

; #%app and #%datum have to be present to make #%top work
(define #'(my-top . id)
  #'(begin
      (displayln (format "got unbound identifier: ~a" 'id))
      (procedure-rename (λ xs (cons 'id xs)) (string->symbol (format "undefined:~a" 'id)))))

(define-inverting #'(tst-program _arg ...)
  #'(begin 
      _arg ...))

(define-for-syntax output-here #'output-here)

(define-inverting #'(header-expr (_filename-string _procname) (_colid ... _outid) ";")
  (inject-syntax ([#'shared-procname (shared-syntax #'_procname)]
                  [#'output (shared-syntax 'output)])
    #'(begin
        (provide (all-defined-out))
        (define shared-procname (dynamic-require (findf file-exists? (list _filename-string (format "~a.rkt" _filename-string))) 'shared-procname))
        (display-header '_colid ... '_outid)
        (define _colid (make-parameter 0)) ...
        (define (_outid)
          (keyword-apply shared-procname
                         (map (compose1 string->keyword symbol->string) (list '_colid ...))
                         (list (_colid) ...) null))
        
        (define (output)
          (display-values (_colid) ... (_outid))))))

(define-inverting #'(load-expr "load" (_filename-string _procname) ",")
  #'(_filename-string _procname))

(define #'(filename _filename)
  (inject-syntax ([#'filename-string (symbol->string (syntax->datum #'_filename))]
                  [#'proc-name (string->symbol (cadr (regexp-match #rx"^(.*)\\.hdl$"(symbol->string (syntax->datum #'_filename)))))])
                 #'(filename-string proc-name)))

(define-inverting #'(table-expr "output-list" _column-id ...)
  #'(_column-id ...))

(define-cases #'column-id
  [#'(_ _colid) #'_colid]
  [#'(_ _colid ",") #'_colid])


(define #'(display-header _sym ...)
  #'(begin
      (apply display-values (list _sym ...))
      (apply display-dashes (list _sym ...))))

(define (vals->text vals)
  (string-join (map ~a vals) " | "))

(define (display-values . vals)
  (displayln (vals->text vals)))

(define (display-dashes . vals)
  (displayln (make-string (string-length (vals->text vals)) #\-)))


(define-inverting #'(test-expr _step-expr ... ";")
  #'(begin
      _step-expr ...))


(define-cases #'step-expr
  [#'(_ _step) #'_step]
  [#'(_ _step ",") #'_step])


(define #'(set-expr "set" _id _val)
  #'(_id _val))


(define #'(eval-expr "eval")
  #'(void))


(define #'(output-expr "output")
  (inject-syntax ([#'output (shared-syntax 'output)])
  #'(output)))

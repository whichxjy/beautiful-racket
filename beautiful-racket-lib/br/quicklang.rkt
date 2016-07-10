#lang br
(provide (except-out (all-from-out br) #%module-begin)
         (rename-out [quicklang-mb #%module-begin]))

(define-macro (quicklang-mb . exprs)
  (define-values
    (kw-pairs other-exprs)
    (let loop ([kw-pairs null][exprs (syntax->list #'exprs)])
      (if (and (pair? exprs) (keyword? (syntax-e (car exprs))) (symbol? (syntax-e (cadr exprs))))
          (loop (cons (list (string->symbol (keyword->string (syntax-e (car exprs))))
                            (cadr exprs)) ; leave val in stx form so local binding is preserved
                      kw-pairs)
                (cddr exprs))
          (values kw-pairs exprs))))
  (with-pattern ([((KW VAL) ...) kw-pairs])
    #`(#%module-begin
       (provide (rename-out [VAL KW]) ...)
       (provide #%top #%app #%datum #%top-interaction)
       . #,(datum->syntax #'exprs other-exprs #'exprs))))


(module reader syntax/module-reader
  #:language 'br/quicklang
  #:info br-get-info
  (require br/get-info))
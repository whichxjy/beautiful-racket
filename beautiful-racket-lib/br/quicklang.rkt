#lang br
(require (for-syntax racket/list sugar/debug))
(provide (except-out (all-from-out br) #%module-begin)
         (rename-out [quicklang-mb #%module-begin]))

(define-macro (quicklang-mb . EXPRS)
  (define-values
    (kw-pairs other-exprs)
    (let loop ([kw-pairs null][exprs (syntax->list #'EXPRS)])
      (if (and (pair? exprs) (keyword? (syntax-e (car exprs))))
          (loop (cons (cons (string->symbol (keyword->string (syntax-e (car exprs))))
                            (cadr exprs)) ; leave val in stx form so local binding is preserved
                      kw-pairs)
                (cddr exprs))
          (values kw-pairs exprs))))
  (define reserved-keywords '(provide))
  (define (reserved? kw-pair) (memq (car kw-pair) reserved-keywords))
  (define-values (reserved-kwpairs other-kwpairs) (partition reserved? kw-pairs))
  (with-pattern ([((KW . VAL) ...) other-kwpairs]
                 [(PROVIDED-ID ...) (or (assq 'provide reserved-kwpairs) null)])
    #`(#%module-begin
       (provide PROVIDED-ID ...)
       (provide (rename-out [VAL KW]) ...)
       (provide #%top #%app #%datum #%top-interaction)
       . #,(datum->syntax #'EXPRS other-exprs #'EXPRS))))


(module reader syntax/module-reader
  #:language 'br/quicklang
  #:info br-get-info
  (require br/get-info))
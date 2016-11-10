#lang br/quicklang
(require json)
(define-macro (js-module-begin PARSE-TREE)
  #'(#%module-begin
     (define result-string PARSE-TREE)
     (define validated-jsexpr (string->jsexpr result-string))
     (display (jsexpr->string validated-jsexpr))))
(provide (rename-out [js-module-begin #%module-begin]))

(define-macro (jsonic-program S-EXP-OR-JSON-CHAR ...)
  #'(string-trim (string-append S-EXP-OR-JSON-CHAR ...)))
(provide jsonic-program)

(define-macro (json-char CHAR-STR) #'CHAR-STR)
(provide json-char)

(define-macro (s-exp SEXP-STR)
  (with-pattern ([SEXP-DATUM (format-datum '~a (syntax->datum #'SEXP-STR))])
    #'(jsexpr->string SEXP-DATUM)))
(provide s-exp)
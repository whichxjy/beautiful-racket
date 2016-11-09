#lang br/quicklang

(require json)
(define-macro (js-module-begin PARSE-TREE)
  #'(#%module-begin
     (define result-string PARSE-TREE)
     (when (string->jsexpr result-string)
       (display result-string))))
(provide (rename-out [js-module-begin #%module-begin]))

(define-macro (jsonic-program S-EXP-OR-JSON-CHAR ...)
  #'(string-trim (string-append S-EXP-OR-JSON-CHAR ...)))
(provide jsonic-program)

(define-macro (json-char TOKEN)
  #'TOKEN)
(provide json-char)

(define (list->json x)
  (format "[~a]" (string-join (map ->string x) ", ")))

(define (hash->json x)
  (format "{~a}" (string-join (for/list ([(k v) (in-hash x)])
                                        (format "~a: ~a" (->string k) (->string v))) ", ")))

(define (->string x)
  (cond
    [(number? x) (number->string x)]
    [(string? x) (format "~v" x)]
    [(list? x) (list->json x)]
    [(hash? x) (hash->json x)]
    [else ""]))

(define-macro (s-exp EXP-STRING)
  (with-pattern ([EXP-DATUM (format-datum '~a (syntax->datum #'EXP-STRING))])
    #'(->string EXP-DATUM)))
(provide s-exp)
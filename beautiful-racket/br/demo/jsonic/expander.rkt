#lang br/quicklang

(require json)
(define-macro (jsonic-mb PARSE-TREE)
  #'(#%module-begin
     (define json-string PARSE-TREE)
     (when (string->jsexpr json-string)
       (display json-string))))
(provide (rename-out [jsonic-mb #%module-begin]))

(define-macro (jsonic-program TOK ...)
  #'(string-trim (string-append TOK ...)))
(provide jsonic-program)

(define-macro (json-char TOK)
  #'TOK)
(provide json-char)

(define (stringify result)
  (cond
    [(number? result) (number->string result)]
    [(string? result) (format "~v" result)]
    [(list? result) (format "[~a]" (string-join (map stringify result) ", "))]
    [(hash? result) (format "{~a}" (string-join (for/list ([(k v) (in-hash result)])
                                                          (format "~a: ~a" (stringify k) (stringify v))) ", "))]
    [else ""]))

(require (for-syntax br/datum racket/string))
(define-macro (s-exp TOK ...)
  (define s-exp-string
    (string-join (map syntax->datum (syntax->list #'(TOK ...))) ""))
  (with-pattern ([DATUM (format-datum '~a s-exp-string)])
    #'(stringify DATUM)))
(provide s-exp)

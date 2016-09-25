#lang br/quicklang
(require json (for-syntax br/datum racket/string))
(provide (rename-out [jsonic-mb #%module-begin])
         jsonic-program
         char
         s-val)
 
(define-macro (jsonic-mb PARSE-TREE)
  #'(#%module-begin
     (define json-string (string-trim PARSE-TREE))
     (when (string->jsexpr json-string)
       (display json-string))))

(define-macro (jsonic-program STR ...)
  #'(string-append STR ...))

(define-macro (char STR)
  #'STR)

(define (stringify result)
  (cond
    [(number? result) (number->string result)]
    [(string? result) (format "~v" result)]
    [(list? result) (format "[~a]" (string-join (map stringify result) ", "))]
    [(hash? result) (format "{~a}" (string-join (for/list ([(k v) (in-hash result)])
                                                          (format "~a: ~a" (stringify k) (stringify v))) ", "))]))

(define-macro (s-val STR ...)
  (define s-exp-string
    (string-join (map syntax->datum (syntax->list #'(STR ...))) ""))
  (with-pattern ([DATUM (format-datum '~a s-exp-string)])
    #'(stringify DATUM)))

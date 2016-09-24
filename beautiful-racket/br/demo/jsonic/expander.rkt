#lang br/quicklang
(provide (rename-out [jsonic-mb #%module-begin])
         jsonic-program
         char
         s-val)
 
(define-macro (jsonic-mb PARSE-TREE)
  #'(#%module-begin
     (display (string-trim PARSE-TREE))))

(define-macro (jsonic-program STR ...)
  #'(string-append STR ...))

(define-macro (char TOK)
  #'TOK)

(define (stringify result)
  (cond
    [(number? result) (number->string result)]
    [(string? result) (format "~v" result)]
    [(list? result) (format "[~a]" (string-join (map stringify result) ", "))]
    [(hash? result) (format "{~a}" (string-join (for/list ([(k v) (in-hash result)])
                                                          (format "~a: ~a" (stringify k) (stringify v))) ", "))]
    [else (error 'unknown-thing)]))

(define-macro (s-val TOK ...)
  (with-pattern ([DATUM (read (open-input-string (apply string-append (map syntax->datum (syntax->list #'(TOK ...))))))])
    #'(stringify DATUM)))

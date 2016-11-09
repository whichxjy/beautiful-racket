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

(define-macro (s-exp TOKEN ...)
  (define token-stxs (syntax->list #'(TOKEN ...)))
  (define token-strs (map syntax->datum token-stxs))
  (define s-exp-str (apply string-append token-strs))
  (with-pattern ([S-EXP-DATUM (format-datum '~a s-exp-str)])
    #'(->json S-EXP-DATUM)))
(provide s-exp)

(define (->json x)
  (cond
    [(number? x) (number->string x)]
    [(string? x) (format "~v" x)]
    [(list? x)
     (format "[~a]" (string-join (map ->json x) ", "))]
    [(hash? x)
     (define pair-strs (for/list ([(k v) (in-hash x)])
                             (format "~a: ~a"
                                     (->json k) (->json v))))
     (format "{~a}" (string-join pair-strs ", "))]
    [else ""]))
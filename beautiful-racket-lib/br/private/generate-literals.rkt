#lang racket/base
(require racket/list "syntax-flatten.rkt")
(provide (all-defined-out))

(define (upcased-and-capitalized? sym)
    (define str (symbol->string sym))
    (and (equal? (string-upcase str) str)
         (let ([first-letter (substring str 0 1)])
           (or (and (string->number first-letter) #t) ; leading digit OK
               (not (equal? (string-downcase first-letter) first-letter))))))

  (define (generate-literals pats)
    ;; generate literals for any symbols that are not ... or _ 
    (define pattern-arg-prefixer "_")
    (for*/list ([pat-arg (in-list (syntax-flatten pats))]
                [pat-datum (in-value (syntax->datum pat-arg))]
                #:when (and (symbol? pat-datum)
                            (not (member pat-datum '(... _))) ; exempted from literality
                            (not (upcased-and-capitalized? pat-datum))))
               pat-arg))
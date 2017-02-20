#lang racket/base
(require "syntax-flatten.rkt")
(provide (all-defined-out))

;; generate literals for any symbols that are not ... or _ and not IN_CAPS
(define (generate-literals pats)
  (for*/list ([pat-arg (in-list (syntax-flatten pats))]
              [pat-datum (in-value (syntax->datum pat-arg))]
              #:when (and (symbol? pat-datum)
                          (not (memq pat-datum '(... _))) ; exempted from literality
                          (let ([pat-str (symbol->string pat-datum)])
                            (not (equal? (string-upcase pat-str) pat-str)))))
    pat-arg))

(module+ test
  (require rackunit)
  (check-equal? (map syntax->datum (generate-literals #'(foo 42 BAR _ bar 3bar))) '(foo bar 3bar)))
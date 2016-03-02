#lang br
(provide (rename-out [simple-module-begin #%module-begin])
          #%datum #%top-interaction)
(define #'(simple-module-begin expr ...)
  #'(#%module-begin
     (define lines (list expr ...))
     (display (format "Expressions = ~a" (length lines)))
     (define numbers (filter number? lines))
     (unless (zero? (length numbers))
       (displayln (format ", numbers = ~a" (length numbers)))
       (apply + numbers))))
#lang br
(provide read-syntax)

(define (read-syntax src in)
  (syntax-let ([#'src-str (port->string in)])
              #'(module no-name racket
                  (define (scramble str)
                    (list->string (shuffle (string->list str))))
                  (regexp-replace* #px"\\w+" src-str scramble))))

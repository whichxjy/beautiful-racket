#lang br
(provide b-print)
(define (b-print . vals)
  (displayln (string-append* (map ~a vals))))

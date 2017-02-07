#lang br
(provide b-input)

(define-macro (b-input ID)
  #'(set! ID (let* ([str (read-line)]
                  [num (string->number (string-trim str))])
             (or num str))))
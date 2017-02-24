#lang br
(require "struct.rkt")
(provide b-rem b-print b-let b-input b-statement b-require)

(define (b-rem val) (void))

(define (b-print . vals)
  (displayln (string-append* (map ~a vals))))

(define-macro (b-let ID VAL) #'(set! ID VAL))

(define-macro (b-input ID)
  #'(b-let ID (let* ([str (read-line)]
                     [num (string->number (string-trim str))])
                (or num str))))

(define-macro (b-statement STMT) #'STMT)

(define-macro (b-require ID) #'(void))
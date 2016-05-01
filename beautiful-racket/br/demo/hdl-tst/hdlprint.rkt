#lang racket


(define (hdlprint val fmt)
  (match-define (list _ radix-letter number-strings) (regexp-match #px"^%(.)(.*)$" fmt)) ; like %B1.16.1
  (match-define (list left-margin width right-margin) (map string->number (string-split number-strings ".")))
  (define radix (case radix-letter
                  [("B") 2]))
  (string-append (make-string left-margin #\space)
                 (if (number? val)
                     (~r val #:min-width width #:pad-string "0" #:base radix)
                     (~a val #:min-width width #:pad-string " " #:align 'center))
                 (make-string right-margin #\space)))

(module+ test
(require rackunit)
(define a 123)
(check-equal? (hdlprint a "%B1.16.1") " 0000000001111011 ")
(check-equal? (hdlprint "out" "%B1.16.1") "       out        "))

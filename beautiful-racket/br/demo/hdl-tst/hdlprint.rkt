#lang racket
(provide hdlprint)

(define (hdlprint val fmt)
  (match-define (list _ radix-letter number-strings) (regexp-match #px"^%(.)(.*)$" fmt)) ; like %B1.16.1
  (match-define (list left-margin width right-margin) (map string->number (string-split number-strings ".")))
  (cond
    [(number? val)
      (define radix (case radix-letter
                     [("B") 2]))
        (string-append (make-string left-margin #\space)
                       (~r val #:min-width width #:pad-string "0" #:base radix)
                       (make-string right-margin #\space))]
    [(string? val) (~a val #:min-width (+ left-margin width right-margin) #:pad-string " " #:align 'center)]
    [else (error 'unknown-value)]))

(module+ test
  (require rackunit)
  (define a 123)
  (check-equal? (hdlprint a "%B1.16.1") " 0000000001111011 ")
  (check-equal? (hdlprint "out" "%B1.16.1") "       out        ")
  (check-equal? (hdlprint "out" "%B3.1.3") "  out  ")
  (check-equal? (hdlprint "in" "%B3.1.3") "  in   "))

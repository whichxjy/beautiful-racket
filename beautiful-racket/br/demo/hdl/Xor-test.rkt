#lang br

#|
load Xor.hdl,
output-list a, b, out;
set a 0, set b 0,
eval, output;
set a 0, set b 1,
eval, output;
set a 1, set b 0,
eval, output;
set a 1, set b 1,
eval, output;
|#

(define (vals->text vals)
  (string-join (map ~a vals) " | "))

(define (display-values . vals)
  (displayln (vals->text vals)))

(define (display-dashes . vals)
  (displayln (make-string (string-length (vals->text vals)) #\-)))

(define #'(display-header _val ...)
  #'(begin
      (apply display-values (list '_val ...))
      (apply display-dashes (list '_val ...))))

(define (display-status)
  (display-values a b (out)))

(define proc (dynamic-require "Xor.hdl" 'Xor))

(display-header a b out)
(define a #f)
(define b #f)
(define (out)
  (keyword-apply proc '(#:a #:b) (list a b) null))


(set! a 0)
(set! b 0)
(display-status)

(set! a 0)
(set! b 1)
(display-status)

(set! a 1)
(set! b 0)
(display-status)

(set! a 1)
(set! b 1)
(display-status)
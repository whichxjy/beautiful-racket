#lang racket
(require "DMux.hdl.rkt")
(require rackunit)

(DMux-in 0)
(DMux-sel 0)
(check-equal? (DMux-a) 0)
(check-equal? (DMux-b) 0)

(DMux-in 0)
(DMux-sel 1)
(check-equal? (DMux-a) 0)
(check-equal? (DMux-b) 0)

(DMux-in 1)
(DMux-sel 0)
(check-equal? (DMux-a) 1)
(check-equal? (DMux-b) 0)

(DMux-in 1)
(DMux-sel 1)
(check-equal? (DMux-a) 0)
(check-equal? (DMux-b) 1)

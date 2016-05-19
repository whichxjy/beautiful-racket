#lang racket
(require "Mux.hdl.rkt")
(require rackunit)

(Mux-sel 0)

(Mux-a 0)
(Mux-b 0)
(check-equal? (Mux-out) (Mux-a))

(Mux-a 0)
(Mux-b 1)
(check-equal? (Mux-out) (Mux-a))

(Mux-a 1)
(Mux-b 0)
(check-equal? (Mux-out) (Mux-a))

(Mux-a 1)
(Mux-b 1)
(check-equal? (Mux-out) (Mux-a))

(Mux-sel 1)

(Mux-a 0)
(Mux-b 0)
(check-equal? (Mux-out) (Mux-b))

(Mux-a 0)
(Mux-b 1)
(check-equal? (Mux-out) (Mux-b))

(Mux-a 1)
(Mux-b 0)
(check-equal? (Mux-out) (Mux-b))

(Mux-a 1)
(Mux-b 1)
(check-equal? (Mux-out) (Mux-b))

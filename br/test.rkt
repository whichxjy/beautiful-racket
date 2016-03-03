#lang racket/base
(provide message)
(define message "You installed beautiful-racket correctly.")
(module+ main
  (displayln message))
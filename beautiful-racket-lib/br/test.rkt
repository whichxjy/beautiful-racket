#lang racket/base
(require setup/getinfo racket/runtime-path)
(provide message)
(define-runtime-path br-dir "../../beautiful-racket")
(define gi (get-info/full br-dir))
(define message (format "You installed beautiful-racket v~a correctly." (gi 'version)))
(module+ main
  (displayln message))

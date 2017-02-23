#lang br
(require (prefix-in basic: (submod "main.rkt" reader)))
(provide current-basic-port configure-repl!)

(define current-basic-port (make-parameter #f))

(define (configure-repl!)
  ;; wrap REPL interactions with pollen expression support
  (define racket-read (current-read-interaction))
  (define (basic-read src in)
    (basic:read-syntax src in))
  (current-read-interaction basic-read))
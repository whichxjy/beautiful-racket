#lang racket/base
(require (for-syntax racket/base))
(provide (all-defined-out))

(define-syntax-rule (until cond expr ...)
  (let loop ()
    (unless cond
      expr ...
      (loop))))

(define-syntax-rule (while cond expr ...)
  (let loop ()
    (when cond
      expr ...
      (loop))))
#lang racket/base
(require (for-syntax racket/base))
(provide (all-defined-out))

(define-syntax-rule (until COND EXPR ...)
  (let loop ()
    (unless COND
      EXPR ...
      (loop))))

(define-syntax-rule (while COND EXPR ...)
  (let loop ()
    (when COND
      EXPR ...
      (loop))))

(define-syntax (forever stx)
  (syntax-case stx ()
    [(_ . EXPRS)
     ;; todo: would be better with a syntax parameter
     (with-syntax ([stop (datum->syntax #'EXPRS 'stop)])
       #'(let/ec stop
           (while #t
                  . EXPRS)))]))

(module+ test
  (require rackunit)
  (check-equal? (let ([x 5])
                  (until (zero? x)
                         (set! x (- x 1)))
                  x) 0)
  (check-equal? (let ([x 5])
                  (while (positive? x)
                         (set! x (- x 1)))
                  x) 0))


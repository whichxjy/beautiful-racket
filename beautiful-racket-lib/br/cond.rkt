#lang racket/base
(require (for-syntax racket/base br/syntax)
         br/define)
(provide (all-defined-out))

(define-macro (until COND EXPR ...)
  #'(let loop ()
      (unless COND
        EXPR ...
        (loop))))

(define-macro (while COND EXPR ...)
  #'(let loop ()
      (when COND
        EXPR ...
        (loop))))

(define-macro (forever . EXPRS)
  ;; todo: would be better with a syntax parameter
  (with-pattern
   ([stop (datum->syntax #'EXPRS 'stop)])
   #'(let/ec stop
       (while #t
              . EXPRS))))

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


#lang br
(require racket/function)
(provide (except-out (all-from-out br) + *)
         (rename-out [my+ +] [my* *]) ⌊)

(define (⌊ largs rargs)
  (let ([lenlargs (length largs)]
        [lenrargs (length rargs)])
    (cond
      [(zero? lenlargs)
       (map (compose1 inexact->exact floor) rargs)]
      [(= lenlargs lenrargs)
       (map min
            largs rargs)])))

(define (my* largs rargs)
  (let ([lenlargs (length largs)]
        [lenrargs (length rargs)])
    (cond
      [(= lenlargs lenrargs)
       (map * largs rargs)]
      [(= 1 lenlargs)
       (map (curry * (car largs)) rargs)]
      [(= 1 lenrargs)
       (my* rargs largs)]
      [else
       (error 'length-error)])))

(define (my+ largs rargs)
  (let ([lenlargs (length largs)]
        [lenrargs (length rargs)])
    (cond
      [(= lenlargs lenrargs)
       (map + largs rargs)]
      [(= 1 lenlargs)
       (map (curry + (car largs)) rargs)]
      [(= 1 lenrargs)
       (my+ rargs largs)]
      [else
       (error 'length-error)])))

(module reader br
  (provide read-syntax)
  (define (read-syntax src-path src-port)
    (define operators '(+ ⌊ *))
    (define src-exprs (for/list ([ln (in-lines src-port)]
                                 #:when (regexp-match #px"\\w" ln))
                                (format-datum '(begin ~a) ln)))
    (with-syntax ([(src-expr ...) src-exprs])
    (syntax->datum #'(module rapl "rapl.rkt"
        (displayln 'src-expr) ...)))))



#;(module+ test
    (require rackunit)
    (check-equal? (+ '(4) '(7)) '(11))
    (check-equal? (+ '(3) '(2 4 11 7 5)) '(5 7 14 10 8))
    (check-equal? (+ '(6 3 8 1) '(3)) '(9 6 11 4))
    (check-equal? (+ '(6 3 8 1) '(3 6 1 8)) '(9 9 9 9))
    (check-exn exn:fail? (λ _ (+ '(6 8 1) '(3 6 1 8)))))
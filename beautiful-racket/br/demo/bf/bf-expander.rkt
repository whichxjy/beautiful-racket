#lang br/quicklang
 
(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))
(provide (rename-out [bf-module-begin #%module-begin]))

(define-macro (bf-program OP-OR-LOOP ...)
  #'(define-values (vec ptr)
      (run-args (list OP-OR-LOOP ...))))
(provide bf-program)

(define (run-args bf-funcs
                  [vec-start (make-vector 30000 0)]
                  [pos-start 0])
  (for/fold ([vec vec-start]
             [pos pos-start])
            ([bf-func (in-list bf-funcs)])
    (bf-func vec pos)))

(define (vector-set v p val)
  (vector-set! v p val)
  v)

(define (vector-update v p func)
  (vector-set v p (func (vector-ref v p))))

(define (gt v p) (values v (add1 p)))
(define (lt v p) (values v (sub1 p)))
(define (plus v p) (values (vector-update v p add1) p))
(define (minus v p) (values (vector-update v p sub1) p))
(define (period v p) (write-byte (vector-ref v p)) (values v p))
(define (comma v p) (values (vector-set v p (read-byte)) p))

(define-macro-cases op
  [(op ">") #'gt]
  [(op "<") #'lt]
  [(op "+") #'plus]
  [(op "-") #'minus]
  [(op ".") #'period]
  [(op ",") #'comma])
(provide op)

(define (make-looping-func args)
  (lambda (v p)
    (for/fold ([vec v]
               [pos p])
              ([i (in-naturals)]
               #:break (zero? (vector-ref vec pos)))
      (run-args args vec pos))))

(define-macro (loop LOOP-ARG ...)
  #'(make-looping-func (list LOOP-ARG ...)))
(provide loop)
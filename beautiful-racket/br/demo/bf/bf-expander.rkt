#lang br/quicklang
 
(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))
(provide (rename-out [bf-module-begin #%module-begin]))

(define-macro (bf-program PROGRAM-ARG ...)
  #'(void (fold-args (list PROGRAM-ARG ...)
                     (make-vector 30000 0)
                     0)))
(provide bf-program)

(define (fold-args bf-args arr ptr)
  (for/fold ([ap (list arr ptr)])
            ([bf-arg (in-list bf-args)])
    (apply bf-arg ap)))

(define (current-byte arr ptr) (vector-ref arr ptr))

(define (set-current-byte arr ptr val)
  (vector-set! arr ptr val)
  arr)

(define (gt arr ptr) (list arr (add1 ptr)))
(define (lt arr ptr) (list arr (sub1 ptr)))
(define (plus arr ptr) (list (set-current-byte arr ptr (add1 (current-byte arr ptr))) ptr))
(define (minus arr ptr) (list (set-current-byte arr ptr (sub1 (current-byte arr ptr))) ptr))
(define (period arr ptr) (write-byte (current-byte arr ptr)) (list arr ptr))
(define (comma arr ptr) (list (set-current-byte arr ptr (read-byte)) ptr))

(define-macro-cases op
  [(op ">") #'gt]
  [(op "<") #'lt]
  [(op "+") #'plus]
  [(op "-") #'minus]
  [(op ".") #'period]
  [(op ",") #'comma])
(provide op)

(define-macro (loop LOOP-ARG ...)
  #'(lambda (arr ptr)
      (for/fold ([ap (list arr ptr)])
                ([i (in-naturals)]
                 #:break (zero? (apply current-byte ap)))
        (apply fold-args (list LOOP-ARG ...) ap))))
(provide loop)

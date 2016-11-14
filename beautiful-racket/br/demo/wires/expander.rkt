#lang br/quicklang
(provide #%module-begin wire
         AND OR LSHIFT RSHIFT NOT)

(define-macro-cases wire
  [(wire ARG -> ID) #'(define/display (ID)
                        (val ARG))]
  [(wire OP ARG -> ID) #'(define/display (ID)
                           (OP (val ARG)))]
  [(wire ARG1 OP ARG2 -> ID) #'(define/display (ID)
                                 (OP (val ARG1) (val ARG2)))]
  [else #'(void)])

(define-macro (define/display (ID) BODY)
  #'(begin
      (define (ID) BODY)
      (module+ main
        (displayln (format "~a: ~a" 'ID (ID))))))

(define val
  (let ([wire-cache (make-hash)])
    (λ (num-or-wire)
      (if (number? num-or-wire)
          num-or-wire
          (hash-ref! wire-cache num-or-wire num-or-wire)))))

(define 16bit-max (expt 2 16))
(define-macro (define-16bit ID+ARGS BODY)
  #'(define ID+ARGS (modulo BODY 16bit-max)))

(define-16bit (AND x y) (bitwise-and x y))
(define-16bit (OR x y) (bitwise-ior x y))
(define-16bit (LSHIFT x y) (arithmetic-shift x y))
(define-16bit (RSHIFT x y) (LSHIFT x (- y)))
(define-16bit (NOT arg) (bitwise-not arg))
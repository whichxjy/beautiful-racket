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

(define (mod-16bit x)
  (define max-16bit (expt 2 16))
  (define remainder (modulo x max-16bit))
  (if (not (negative? remainder))
      remainder
      (mod-16bit (+ max-16bit remainder))))

(define-macro (define-16bit ID+ARGS BODY)
  #'(define ID+ARGS (mod-16bit BODY)))

(define-16bit (AND arg1 arg2) (bitwise-and arg1 arg2))
(define-16bit (OR arg1 arg2) (bitwise-ior arg1 arg2))
(define-16bit (LSHIFT arg1 arg2) (arithmetic-shift arg1 arg2))
(define-16bit (RSHIFT arg1 arg2) (arithmetic-shift arg1 (- arg2)))
(define-16bit (NOT arg) (bitwise-not arg))
#lang br/quicklang
(require "main.rkt")
(provide (all-from-out "main.rkt"))

(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define wire-datums
    (for/list ([wire-str (in-lines port)])
      (format-datum '(wire ~a) wire-str)))
  (strip-bindings
   #`(module wires-mod wires-demo/with-promises
       #,@wire-datums)))

(define-macro-cases wire
  [(wire ARG -> WIRE) #'(define/display (WIRE)
                          (val ARG))]
  [(wire OP ARG -> WIRE) #'(define/display (WIRE)
                             (OP (val ARG)))]
  [(wire ARG1 OP ARG2 -> WIRE) #'(define/display (WIRE)
                                   (OP (val ARG1) (val ARG2)))]
  [else #'(void)])
(provide wire)

(define-macro (define/display (WIRE) BODY)
  #'(begin
      (define WIRE (delay BODY))
      (module+ main
        (displayln 'promises)
        (displayln (format "~a: ~a" 'WIRE (val WIRE))))))

(define (val num-or-wire) (force num-or-wire))
#lang br/quicklang
(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define wire-datums
    (for/list ([wire-str (in-lines port)])
              (format-datum '(wire ~a) wire-str)))
  (strip-context
   #`(module wires-mod br/demo/wires/expander
       #,@wire-datums)))
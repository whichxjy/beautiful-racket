#lang br/quicklang
(module+ reader
  (provide read-syntax))

(define (read-syntax path port)
  (define wire-strs (port->lines port))
  (define wire-datums (format-datums '(wire ~a) wire-strs))
  (strip-context
   #`(module wires-mod br/demo/wires/expander
       #,@wire-datums)))
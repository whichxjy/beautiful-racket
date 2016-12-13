#lang br/quicklang
(module reader br
  (require "reader.rkt")
  (provide read-syntax get-info)
  (define (get-info port mod line col pos)
    (define (handle-query key default)
      (case key
        [(color-lexer)
         (dynamic-require 'br/demo/jsonic-2/colorer 'color-jsonic)]
        [(drracket:indentation)
         (dynamic-require 'br/demo/jsonic-2/indenter 'indent-jsonic)]
        [(drracket:toolbar-buttons)
         (dynamic-require 'br/demo/jsonic-2/buttons 'make-buttons)]
        [else default]))
    handle-query))


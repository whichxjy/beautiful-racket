#lang br/quicklang
(module reader br
  (require "reader.rkt" "colorer.rkt"
           "indenter.rkt" "buttons.rkt")
  (provide read-syntax get-info)
  (define (get-info port mod line col pos)
    (define (handle-query key default)
      (case key
        [(color-lexer) color-jsonic]
        [(drracket:indentation) indent-jsonic]
        [(drracket:toolbar-buttons) make-jsonic-buttons]
        [else default]))
    handle-query))


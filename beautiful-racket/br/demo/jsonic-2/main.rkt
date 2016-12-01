#lang br/quicklang
(module reader br
  (require "reader.rkt")
  (provide read-syntax))

#|
Demonstrate:
+ contracts
+ unit tests
+ color lexing
+ indentation
+ toolbar buttons
+ docs
+ info.rkt
|#


(define (get-info . _)
  (λ (key default)
    (case key
      [(color-lexer)
       (dynamic-require 'br/demo/jsonic/color-lexer 'color-lexer (λ () #f))]
      [(drracket:indentation)
       (dynamic-require 'br/demo/jsonic/indenter 'indenter (λ () #f))]
      [(drracket:toolbar-buttons)
       (dynamic-require 'br/demo/jsonic/toolbar 'buttons (λ () #f))]
      [else default])))
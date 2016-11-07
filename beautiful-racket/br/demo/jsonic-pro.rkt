#lang br/quicklang
(module reader br/quicklang
  (require (submod "jsonic-pro/main.rkt" reader))
  (provide (all-from-out (submod "jsonic-pro/main.rkt" reader))))

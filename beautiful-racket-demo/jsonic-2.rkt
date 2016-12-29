#lang br/quicklang
(module reader br/quicklang
  (require (submod "jsonic-2/main.rkt" reader))
  (provide (all-from-out (submod "jsonic-2/main.rkt" reader))))

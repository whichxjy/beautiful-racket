#lang br/quicklang
(module reader br/quicklang
  (require (submod "wires/main.rkt" reader))
  (provide (all-from-out (submod "wires/main.rkt" reader))))

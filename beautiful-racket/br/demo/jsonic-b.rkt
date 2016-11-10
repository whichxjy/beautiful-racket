#lang br/quicklang
(module reader br/quicklang
  (require (submod "jsonic-b/main.rkt" reader))
  (provide (all-from-out (submod "jsonic-b/main.rkt" reader))))

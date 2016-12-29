#lang br/quicklang
(module reader br/quicklang
  (require (submod "jsonic/main.rkt" reader))
  (provide (all-from-out (submod "jsonic/main.rkt" reader))))

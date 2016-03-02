#lang racket
(provide (all-from-out racket))

(module reader syntax/module-reader
  br/ex/nothing/nothing)

#|

#lang racket
(provide (all-from-out racket))

(module reader racket/base
  (provide read read-syntax))

|#

#|

(module nothing racket
  (provide (all-from-out racket))
  
  (module reader syntax/module-reader
    br/ex/nothing))

|#

#|

#lang racket

(module reader syntax/module-reader
  #:language '(submod br/ex/nothing semantics))

(module semantics racket
  (provide (all-from-out racket)))

(sleep 100000)

|#

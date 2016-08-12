#lang racket
(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [loader-module-begin #%module-begin]))

#|

br/load makes it possible to invoke a quick #lang by its pathname (without installing it as a collection)

#lang br/load "path.rkt"

Should simply delegate the reader & semantics.

|#

(define-syntax-rule (loader-module-begin loadpath expr ...)
  (#%module-begin
   (module loader-module loadpath
     expr ...)
   (require 'loader-module)
   
   (module reader racket/base
     (require '(submod loadpath reader))
     (provide (all-from-out '(submod loadpath reader))))))

(module reader syntax/module-reader
  br/load)
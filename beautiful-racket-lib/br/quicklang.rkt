#lang br
(require (for-syntax racket/list sugar/debug))
(provide (except-out (all-from-out br) #%module-begin)
         (rename-out [quicklang-mb #%module-begin])
         (for-syntax (all-from-out sugar/debug)))

(define-macro (quicklang-mb . EXPRS)
  #'(#%module-begin
     (provide #%top #%app #%datum #%top-interaction)
     . EXPRS))

(module reader syntax/module-reader
  #:language 'br/quicklang
  #:info br-get-info
  #:read at:read
  #:read-syntax at:read-syntax
  (require br/get-info (prefix-in at: scribble/reader)))
#lang br
(provide (except-out (all-from-out br) #%module-begin)
         (rename-out [quicklang-mb #%module-begin]))

(define-syntax-rule (quicklang-mb . lines)
  (#%module-begin
     (provide #%top #%app #%datum #%top-interaction)
     . lines))


(module reader syntax/module-reader
  #:language 'br/quicklang
  #:info br-get-info
  (require br/get-info))
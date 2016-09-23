#lang br/quicklang
 
(define-macro (jsonic-mb PARSE-TREE)
  #'(#%module-begin
     'PARSE-TREE))
(provide (rename-out [jsonic-mb #%module-begin]))


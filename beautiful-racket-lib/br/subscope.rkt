#lang br
(provide (all-defined-out))

(define #'(introduce-scope _color . _parents)
  (with-syntax ([color #'_color]
                [color-string (format-string "~a" #'_color)]
                [color:define (suffix-id #'_color ":define")]
                [color:id (suffix-id #'_color ":id")]
                [color-binding-id (suffix-id #'_color "-binding-id")]
                [color-binding-form (suffix-id  #'_color "-binding-form")]
                [color-id (suffix-id #'_color "-id")]
                [module-name (generate-temporary)]
                [parents (if (pair? (syntax->list #'_parents))
                             (car (syntax->list #'_parents))
                             (syntax->list #'_parents))])
    (replace-context #'_color
                     #'(begin
                         (module module-name br
                           (require (for-syntax br/datum br/scope))
                           (provide (for-syntax (all-defined-out)) (all-defined-out))
                           
                           (begin-for-syntax
                             (define-scope color parents))
                           
                           (define #'(color:define _id-in _expr)
                             (inject-syntax* ([#'color:id (shared-syntax (prefix-id color-string ":" #'_id-in))]
                                              [#'color-binding-id (color-binding-form #'_id-in)]
                                              [#'color-id (color #'color-binding-id)])
                                             #'(begin
                                                 (define color-binding-id _expr)
                                                 (define-syntax color:id
                                                   (syntax-id-rules ()
                                                     [_ color-id]))))))
                         (require 'module-name)))))

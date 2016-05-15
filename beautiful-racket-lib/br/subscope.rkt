#lang br
(provide (all-defined-out))

(define #'(introduce-scope _color . _parents)
  (with-syntax ([color #'_color]
                [color-string (format "~a" (syntax->datum #'_color))]
                [color:define (format-id #f "~a:define" #'_color)]
                [color:id (format-id #f "~a:id" #'_color)]
                [color-binding-id (format-id #f "~a-binding-id" #'_color)]
                [color-binding-form (format-id #f "~a-binding-form" #'_color)]
                [color-id (format-id #f "~a-id" #'_color)]
                [module-name (generate-temporary)]
                [parents (if (pair? (syntax->list #'_parents))
                             (car (syntax->list #'_parents))
                             (syntax->list #'_parents))])
    (replace-context #'_color
                     #'(begin
                         (module module-name br
                           (require (for-syntax br/datum br/syntax))
                           (provide (for-syntax (all-defined-out)) (all-defined-out))
                           
                           (begin-for-syntax
                             (define-scope color parents))
                           
                           (define #'(color:define _id-in _expr)
                             (with-syntax* ([color:id (shared-syntax (format-datum "~a:~a" color-string #'_id-in))]
                                            [color-binding-id (color-binding-form #'_id-in)]
                                            [color-id (color #'color-binding-id)])
                               #'(begin
                                   (define color-binding-id _expr)
                                   (define-syntax color:id
                                     (syntax-id-rules ()
                                       [_ color-id]))))))
                         (require 'module-name)))))

#lang br
(provide (all-defined-out))

(define #'(introduce-scope _color . _parents)
  (replace-context #'_color
                   (with-syntax ([color #'_color]
                                 [color-string (format "~a" (syntax->datum #'_color))]
                                 [define:color (format-id #f "define:~a" #'_color)]
                                 [id:color (format-id #f "id:~a" #'_color)]
                                 [color-binding-id (format-id #f "~a-binding-id" #'_color)]
                                 [color-binding-form (format-id #f "~a-binding-form" #'_color)]
                                 [color-id (format-id #f "~a-id" #'_color)]
                                 [module-name (generate-temporary)]
                                 [parents (if (pair? (syntax->list #'_parents))
                                              (car (syntax->list #'_parents))
                                              (syntax->list #'_parents))])
                     #'(begin
                         (module module-name br
                           (require (for-syntax br/datum br/syntax))
                           (provide (for-syntax (all-defined-out)) (all-defined-out))
                           
                           (begin-for-syntax
                             (define-scope color parents))
                           
                           (define #'(define:color _id-in _expr)
                             (with-syntax* ([id:color (shared-syntax (format-datum "~a:~a" #'_id-in color-string))]
                                            [color-binding-id (color-binding-form #'_id-in)]
                                            [color-id (color #'color-binding-id)])
                               #'(begin
                                   (define color-binding-id _expr)
                                   (define-syntax id:color
                                     (syntax-id-rules ()
                                       [_ color-id]))))))
                         (require 'module-name)))))

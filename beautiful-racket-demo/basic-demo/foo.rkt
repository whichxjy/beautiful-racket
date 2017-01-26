#lang br
(provide (rename-out [my-app #%app]))
(define-macro (my-app ID . ARGS)
  (report caller-stx)
  (if (number? (syntax->datum #'ID))
      (with-pattern ([NEW-ID (report* #'ID (prefix-id "@" #'ID))])
        #'(NEW-ID . ARGS))
      #'(ID . ARGS)))

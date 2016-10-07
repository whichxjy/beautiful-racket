#lang br
(require racket/class)
(provide drracket-indenter)

(define (drracket-indenter t pos)
  #;(with-handlers ([exn:fail? (λ(exn) #f)]) ; this function won't work until gui-lib 1.26
    (send t compute-racket-amount-to-indent pos (λ(x)
                                                  (case x
                                                    [("with-pattern" "with-shared-id") 'lambda]
                                                    [("define-macro") 'define]
                                                    [else #f]))))
  #f)
#lang racket
(require racket/class)
(provide (all-defined-out))

(define (indenter t pos)
  (with-handlers ([exn:fail? (λ (exn) #f)]) ; this function won't work until gui-lib 1.26
    (send t compute-racket-amount-to-indent pos (λ(x)
                                                  (case x
                                                    [("with-pattern"
                                                      "with-shared-id") 'lambda]
                                                    [("define-macro"
                                                      "define-macro-cases"
                                                      "define-cases") 'define]
                                                    [else #f])))))

(define (br-get-info key default default-filter)
  (case key
    [(drracket:indentation) indenter]
    [else (default-filter key default)]))
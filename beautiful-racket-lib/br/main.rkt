#lang racket/base
(require racket/provide racket/list racket/string racket/format racket/match racket/port
         br/define br/syntax br/datum br/debug br/conditional racket/function
         (for-syntax racket/base racket/syntax br/syntax br/debug br/define))
(provide (except-out (all-from-out racket/base) define)
         (all-from-out racket/list racket/string racket/format racket/match racket/port
                       br/syntax br/datum br/debug br/conditional racket/function)
         (for-syntax (all-from-out racket/base racket/syntax br/syntax br/debug))
         (for-syntax caller-stx shared-syntax with-shared-id with-calling-site-id) ; from br/define
         (filtered-out
          (λ (name)
            (let ([pat (regexp "^br:")])
              (and (regexp-match? pat name)
                   (regexp-replace pat name ""))))
          (combine-out (all-from-out br/define))))

;; todo: activate at-exp reader by default

(define (remove-blank-lines strs)
  (filter (λ(str) (regexp-match #px"\\S" str)) strs))

(provide remove-blank-lines)


(module reader syntax/module-reader
  #:language 'br
  #:info my-get-info
  
  (require racket/class)
  (define (indenter t pos)
    (with-handlers ([exn:fail? (λ(exn) #f)]) ; this function won't work until gui-lib 1.26
      (send t compute-racket-amount-to-indent pos (λ(x)
                                                    (case x
                                                      [("with-pattern" "with-shared-id") 'lambda]
                                                      [("define-macro") 'define]
                                                      [else #f])))))
  
  (define (my-get-info key default default-filter)
    (case key
      #;[(color-lexer)
         (dynamic-require 'syntax-color/default-lexer 'default-lexer)]
      [(drracket:indentation) indenter]
      [else
       (default-filter key default)])))
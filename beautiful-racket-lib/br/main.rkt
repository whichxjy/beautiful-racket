#lang racket/base
(require racket/provide racket/list racket/string racket/format racket/match racket/port
         br/define br/syntax br/datum br/debug br/cond racket/function
         (for-syntax racket/base racket/syntax br/syntax br/debug br/define))
(provide (all-from-out racket/base)
         (all-from-out racket/list racket/string racket/format racket/match racket/port
                       br/syntax br/datum br/debug br/cond racket/function br/define)
         (for-syntax (all-from-out racket/base racket/syntax br/syntax br/debug))
         (for-syntax caller-stx shared-syntax with-shared-id with-calling-site-id)) ; from br/define
         

;; todo: activate at-exp reader by default

(define (remove-blank-lines strs)
  (filter (λ(str) (regexp-match #px"\\S" str)) strs))

(provide remove-blank-lines)


(module reader syntax/module-reader
  #:language 'br
  #:info br-get-info
  (require br/get-info))
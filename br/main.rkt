#lang racket/base
(require racket/provide racket/list racket/string racket/format racket/match racket/port
         br/define br/syntax br/datum br/debug
         (for-syntax racket/base racket/syntax br/syntax br/define))
(provide (except-out (all-from-out racket/base) define)
         (all-from-out racket/list racket/string racket/format racket/match racket/port
                       br/syntax br/datum br/debug)
         (for-syntax (all-from-out racket/base racket/syntax br/syntax))
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
  #:language 'br)
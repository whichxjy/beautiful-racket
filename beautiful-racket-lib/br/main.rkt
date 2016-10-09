#lang racket/base
(require racket/provide racket/list racket/string racket/format racket/match racket/port
         br/define br/syntax br/datum br/debug br/cond racket/class
         (for-syntax racket/base racket/syntax br/syntax br/debug br/define))
(provide (all-from-out racket/base)
         (all-from-out racket/list racket/string racket/format racket/match racket/port
                       br/syntax br/datum br/debug br/cond racket/class br/define)
         (for-syntax (all-from-out racket/base racket/syntax br/syntax br/debug))
         (for-syntax caller-stx with-shared-id)) ; from br/define
         
;; todo: activate at-exp reader by default

(provide evaluate)
(define-macro (evaluate DATUM)
  #'(begin
      (define-namespace-anchor nsa)
      (eval DATUM (namespace-anchor->namespace nsa))))

(provide really-dynamic-require)
(define-macro (really-dynamic-require . ARGS)
  #'(parameterize ([current-namespace (make-base-namespace)])
      (dynamic-require . ARGS)))

(module reader syntax/module-reader
  #:language 'br
  #:info br-get-info
  (require br/get-info))
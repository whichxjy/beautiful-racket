#lang racket/base
(require racket/provide racket/list racket/string racket/format racket/match racket/port racket/function racket/provide
         br/define br/syntax br/datum br/debug br/cond br/list br/reader-utils br/module racket/class racket/vector
         (for-syntax racket/base racket/syntax br/syntax br/debug br/define br/datum))
(provide (all-from-out racket/base)
         (all-from-out racket/list racket/string racket/format racket/match racket/port racket/function racket/provide
                       br/syntax br/datum br/debug br/cond br/list br/reader-utils br/module racket/class racket/vector br/define)
         (for-syntax (all-from-out racket/base racket/syntax br/syntax br/debug br/datum))
         (for-syntax caller-stx with-shared-id)) ; from br/define
         
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
  #:read at:read
  #:read-syntax at:read-syntax
  (require br/get-info (prefix-in at: scribble/reader)))
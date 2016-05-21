#lang br
(require "helper.rkt" (for-syntax racket/base racket/syntax racket/require-transform br/syntax))
(provide #%top-interaction #%module-begin #%app #%datum and or (all-defined-out))


(define #'(chip-program _chipname
                        (in-spec (_in-bus _in-width ...) ...)
                        (out-spec (_out-bus _out-width ...) ...)
                        _part ...)
  (inject-syntax* ([#'_chip-prefix (suffix-id #'_chipname "-")]
                   [#'(_in-bus-write ...) (suffix-ids #'(_in-bus ...) "-write")]
                   [#'(_prefix-out-bus ...) (prefix-ids #'_chip-prefix #'(_out-bus ...))])
                  #'(begin
                      (provide (prefix-out _chip-prefix (combine-out _in-bus ... _in-bus-write ...))) 
                      (define-input-bus _in-bus _in-width ...) ...
                      _part ...
                      (provide _prefix-out-bus ...)
                      (define-output-bus _prefix-out-bus _out-bus _out-width ...) ...)))


(define #'(part _partname ((_bus-left . _busargs) _bus-expr-right) ...)
  (inject-syntax ([#'(_partname-bus-left ...) (prefix-ids #'_partname "-" #'(_bus-left ...))]
                  [#'_chip-module-path (format-string "~a.hdl.rkt" #'_partname)])
                 #'(begin
                     (require (import-chip _chip-module-path) (for-syntax (import-chip _chip-module-path)))
                     (handle-buses ((_partname-bus-left . _busargs) _bus-expr-right) ...))))


(define-syntax import-chip
  (make-require-transformer
   (λ (stx)
     (syntax-case stx ()
       [(_ module-path)
        (expand-import #'module-path)]))))


(define #'(handle-buses _bus-assignments ...)
  (let-values ([(_in-bus-assignments _out-bus-assignments)
                (syntax-case-partition #'(_bus-assignments ...) ()
                                       [((prefixed-wire . _wireargs) _)
                                        (syntax-local-eval (syntax-shift-phase-level #'(input-bus? prefixed-wire) 1))])])
    (inject-syntax* ([#'(((_in-bus _in-bus-arg ...) _in-bus-value) ...) _in-bus-assignments]
                     [#'(_in-bus-write ...) (suffix-ids #'(_in-bus ...) "-write")]
                     [#'((_out-bus-expr (_new-out-bus)) ...) _out-bus-assignments])
                    #'(begin
                        (define-output-bus _new-out-bus
                          (λ ()
                            (_in-bus-write _in-bus-arg ... _in-bus-value) ...
                            _out-bus-expr)) ...))))
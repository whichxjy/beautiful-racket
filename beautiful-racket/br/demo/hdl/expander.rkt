#lang br
(require "helper.rkt" (for-syntax racket/base racket/syntax "helper.rkt" racket/list racket/require-transform))
(provide #%top-interaction #%module-begin #%app #%datum and or (all-defined-out))


(define #'(chip-program _chipname
                        (in-spec (_input-pin _input-width ...) ...)
                        (out-spec (_output-pin _output-width ...) ...)
                        _part ...)
  (with-syntax* ([chip-prefix (format-id #'_chipname "~a-" #'_chipname)]
                 [(in-pin-write ...) (map (λ(iw) (format-id iw "~a-write" iw)) (syntax->list #'(_input-pin ...)))]
                 [(prefixed-output-pin ...) (map (λ(op) (format-id op "~a~a" #'chip-prefix op)) (syntax->list #'(_output-pin ...)))])
    #'(begin
        (provide (prefix-out chip-prefix (combine-out _input-pin ... in-pin-write ...))) 
        (define-input-bus _input-pin _input-width ...) ...
        _part ...
        (provide prefixed-output-pin ...)
        (define-output-bus prefixed-output-pin _output-pin _output-width ...) ...)))


(define #'(part _prefix ((_wire . _wireargs) _wirevalue) ...)
  (with-syntax ([(prefixed-wire ...) (map (λ(s) (format-id s "~a-~a" #'_prefix s)) (syntax->list #'(_wire ...)))]
                [chip-module-path (datum->syntax #'_prefix (format "~a.hdl.rkt" (syntax->datum #'_prefix)))])
    #'(begin
        (require (import-chip chip-module-path) (for-syntax (import-chip chip-module-path)))
        (handle-wires ((prefixed-wire . _wireargs) _wirevalue) ...))))


(define-syntax import-chip
  (make-require-transformer
   (λ (stx)
     (syntax-case stx ()
       [(_ module-path)
        (expand-import #'module-path)]))))


(define #'(handle-wires _wire-assignments ...)
  (let-values ([(in-wire-stxs out-wire-stxs)
                (partition (λ(wa)
                             (syntax-case wa ()
                               [((prefixed-wire . _wireargs) _)
                                (input-bus? (syntax-local-eval #'prefixed-wire))]))
                           (syntax->list #'(_wire-assignments ...)))])
    (with-syntax* ([(((in-wire in-arg ...) input-expr) ...) in-wire-stxs]
                   [(in-wire-write ...) (map (λ(iw) (format-id iw "~a-write" iw)) (syntax->list #'(in-wire ...)))]
                   [(((out-wire out-arg ...) (out-bus)) ...) out-wire-stxs])
      #'(begin
          (define-output-bus out-bus
            (λ ()
              (in-wire-write in-arg ... input-expr) ...
              (out-wire out-arg ...))) ...))))
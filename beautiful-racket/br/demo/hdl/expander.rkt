#lang br
(require "helper.rkt" (for-syntax racket/base racket/syntax "helper.rkt" racket/list racket/require-transform))
(provide #%top-interaction (rename-out [mb #%module-begin]) #%app #%datum and or (all-defined-out))

(define #'(mb _arg ...)
  #'(#%module-begin
     _arg ...))

(define #'(chip-program _chipname
                        (in-spec (_input-pin _input-width ...) ...)
                        (out-spec (_output-pin _output-width ...) ...)
                        _part ...)
  (with-syntax ([chip-prefix (format-id #'_chipname "~a-" #'_chipname)])
    #'(begin
         (provide (prefix-out chip-prefix (combine-out _input-pin ... _output-pin ...))) 
         (define-input-bus _input-pin _input-width ...) ...
         _part ...)))


(define #'(part _prefix [_suffix _arg ...] ...)
  (with-syntax ([(prefix-suffix ...) (map (λ(s) (format-id s "~a-~a" #'_prefix s)) (syntax->list #'(_suffix ...)))]
                [chip-module-path (datum->syntax #'_prefix (format "~a.hdl.rkt" (syntax->datum #'_prefix)))])
    #'(begin
        (require (import-chip chip-module-path) (for-syntax (import-chip chip-module-path)))
        (handle-wires [prefix-suffix _arg ...] ...))))


(define-syntax import-chip
  (make-require-transformer
   (λ (stx)
     (syntax-case stx ()
       [(_ module-path)
        (expand-import #'module-path)]))))


(define #'(handle-wires _wirearg-pair ...)
  (let-values ([(in-wire-stxs out-wire-stxs)
                (partition (λ(wirearg-pair-stx)
                             (define wire-stx (car (syntax->list wirearg-pair-stx)))
                             (input-bus? (syntax-local-eval wire-stx)))
                           (syntax->list #'(_wirearg-pair ...)))])
    (with-syntax ([([in-wire . in-args] ...) in-wire-stxs]
                  [([out-wire out-arg ... out-bus] ...) out-wire-stxs])
      #'(begin
          (define-output-bus out-bus
            (λ ()
            (in-wire . in-args) ...
            (out-wire out-arg ...))) ...))))
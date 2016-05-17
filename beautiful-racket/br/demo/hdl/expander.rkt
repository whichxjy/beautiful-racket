#lang br
(require "helper.rkt" (for-syntax racket/base racket/syntax "helper.rkt" racket/list racket/require-transform))
(provide #%top-interaction #%module-begin #%app #%datum (all-defined-out))


(define #'(chip-program _chipname
                        (in-spec _input-pin ...)
                        (out-spec _output-pin ...)
                        (part-spec (part _partname (_pin _val) ... ) ...))
  (with-syntax ([chip-prefix (format-id #'_chipname "~a-" #'_chipname)])
    #'(begin
        (provide (prefix-out chip-prefix (combine-out _input-pin ... _output-pin ...))) 
        (define _input-pin (make-input)) ...
        (handle-part _partname (_pin _val) ...) ...)))


(define #'(handle-part _prefix [_suffix _arg] ...)
  (with-syntax ([(prefix-suffix ...) (map (λ(s) (format-id s "~a-~a" #'_prefix s)) (syntax->list #'(_suffix ...)))]
                [chip-module-path (datum->syntax #'_prefix (format "~a.hdl.rkt" (syntax->datum #'_prefix)))])
    #'(begin
        (require (import-chip chip-module-path) (for-syntax (import-chip chip-module-path)))
        (handle-wires [prefix-suffix _arg] ...))))


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
                             (input-wire? (syntax-local-eval wire-stx)))
                           (syntax->list #'(_wirearg-pair ...)))])
    (with-syntax ([([in-wire in-arg] ...) in-wire-stxs]
                  [([out-wire out-arg] ...) out-wire-stxs])
      #'(begin
          (define (out-arg)
            (in-wire (in-arg)) ...
            (out-wire)) ...))))
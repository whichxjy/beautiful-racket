#lang br
(require "helper.rkt" (for-syntax racket/base racket/syntax "helper.rkt" racket/list racket/require-transform))
(provide #%top-interaction (rename-out [mb #%module-begin]) #%app #%datum (all-defined-out))

(define #'(mb _arg ...)
  #'(#%module-begin
     _arg ...))

(define #'(chip-program _chipname
                        (in-spec (_input-pin _input-width ...) ...)
                        (out-spec (_output-pin _output-width ...) ...)
                        . args)
  (with-syntax ([chip-prefix (format-id #'_chipname "~a-" #'_chipname)])
    #''(begin
         (provide (prefix-out chip-prefix (combine-out _input-pin ... _output-pin ...))) 
         (define _input-pin (make-bus '_input-pin _input-width ...)) ...
         . args)))

#;(define #'(chip-program _chipname
                          (in-spec (_input-pin _input-width ...) ...)
                          (out-spec (_output-pin _output-width ...) ...)
                          (part-spec (part _partname ((_pin _pinwhich ...) (_val _valwhich ...)) ... ) ...))
    (with-syntax ([chip-prefix (format-id #'_chipname "~a-" #'_chipname)])
      #''(begin
           (provide (prefix-out chip-prefix (combine-out _input-pin ... _output-pin ...))) 
           (define _input-pin (make-bus _input-width ...)) ...
           #;(define _output-pin (make-bus _output-width ...)) #;...
           (handle-part _partname (_pin (or #f _pinwhich ...) (_val (or #f _valwhich ...))) ...) ...)))


(define #'(handle-part _prefix [_suffix _which _arg] ...)
  (with-syntax ([(prefix-suffix ...) (map (λ(s) (format-id s "~a-~a" #'_prefix s)) (syntax->list #'(_suffix ...)))]
                [chip-module-path (datum->syntax #'_prefix (format "~a.hdl.rkt" (syntax->datum #'_prefix)))])
    #'(begin
        (require (import-chip chip-module-path) (for-syntax (import-chip chip-module-path)))
        (handle-wires [prefix-suffix _which _arg] ...))))


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
    (with-syntax ([([in-wire . in-args] ...) in-wire-stxs]
                  [([out-wire which (out-arg . args)] ...) out-wire-stxs])
      #'(begin
          (define (out-arg)
            (in-wire . in-args) ...
            (out-wire which)) ...))))
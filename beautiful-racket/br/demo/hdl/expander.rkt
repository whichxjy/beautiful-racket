#lang br
(require "helper.rkt" (for-syntax racket/syntax racket/require-transform br/syntax "helper.rkt"))
(provide #%top-interaction #%module-begin #%app #%datum (all-defined-out))

(define-macro (chip-program CHIPNAME
                            (in-spec (IN-BUS IN-WIDTH ...) ...)
                            (out-spec (OUT-BUS OUT-WIDTH ...) ...)
                            PART ...)
  (with-pattern
   ([CHIP-PREFIX (suffix-id #'CHIPNAME "-")]
    [(IN-BUS-WRITE ...) (suffix-id #'(IN-BUS ...) "-write")]
    [(PREFIX-OUT-BUS ...) (prefix-id #'CHIP-PREFIX #'(OUT-BUS ...))])
   #'(begin
       (provide (prefix-out CHIP-PREFIX (combine-out IN-BUS ... IN-BUS-WRITE ...))) 
       (define-input-bus IN-BUS IN-WIDTH ...) ...
       PART ...
       (provide PREFIX-OUT-BUS ...)
       (define-output-bus PREFIX-OUT-BUS OUT-BUS OUT-WIDTH ...) ...)))


(define-macro (part PARTNAME ((BUS-LEFT . BUS-LEFT-ARGS) BUS-RIGHT-EXPR) ...)
  (with-pattern
   ([(PARTNAME-BUS-LEFT ...) (prefix-id #'PARTNAME "-" #'(BUS-LEFT ...))]
    [CHIP-MODULE-PATH (format-string "~a.hdl.rkt" #'PARTNAME)])
   #'(begin
       (require (import-chip CHIP-MODULE-PATH) (for-syntax (import-chip CHIP-MODULE-PATH)))
       (handle-buses ((PARTNAME-BUS-LEFT . BUS-LEFT-ARGS) BUS-RIGHT-EXPR) ...))))


(define-syntax import-chip
  (make-require-transformer
   (λ (stx)
     (syntax-case stx ()
       [(_ module-path)
        (expand-import #'module-path)]))))


(define-macro (handle-buses BUS-ASSIGNMENTS ...)
  (let-values
      ([(in-bus-assignments out-bus-assignments)
        (syntax-case-partition #'(BUS-ASSIGNMENTS ...) ()
                               [((PREFIXED-WIRE . _) _)
                                (input-bus? (syntax-local-eval #'PREFIXED-WIRE))])])
    (with-pattern
     ([(((IN-BUS IN-BUS-ARG ...) IN-BUS-VALUE) ...) in-bus-assignments]
      [(IN-BUS-WRITE ...) (suffix-id #'(IN-BUS ...) "-write")]
      [((OUT-BUS-EXPR (NEW-OUT-BUS)) ...) out-bus-assignments])
     #'(begin
         (define-output-bus NEW-OUT-BUS
           (λ ()
             (IN-BUS-WRITE IN-BUS-ARG ... IN-BUS-VALUE) ...
             OUT-BUS-EXPR)) ...))))
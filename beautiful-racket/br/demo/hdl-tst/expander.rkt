#lang br
(require (for-syntax br/syntax br/scope))
(provide #%top-interaction #%module-begin #%datum #;(rename-out [my-top #%top]) #%app
         (all-defined-out) (all-from-out br))

(require br/demo/hdl-tst/hdlprint rackunit racket/file (for-syntax racket/string))

(define-for-syntax chip-prefix #f)

(define-macro (tst-program ARG ...)
  (let-syntax-pattern ([compare (shared-syntax #'compare)]
                       [of (shared-syntax #'of)])
                      #'(begin ARG ... (close-output-port of) (compare) )))

(define-macro (load-expr CHIPFILE-STRING)
  (let ()
    (set! chip-prefix (string-replace (syntax->datum #'CHIPFILE-STRING) ".hdl" ""))
    (let-syntax-pattern ([CHIPFILE.RKT (format-string "~a.rkt" #'CHIPFILE-STRING)])
                        #'(require CHIPFILE.RKT))))

(define-macro (output-file-expr OUTPUT-FILE-STRING)
  (let-syntax-pattern ([ofname (shared-syntax #'ofname)]
                       [of (shared-syntax #'of)])
                      #'(begin
                          (define ofname OUTPUT-FILE-STRING)
                          (define of (open-output-file ofname #:mode 'text #:exists 'replace)))))

(define-macro (compare-to-expr COMPARE-FILE-STRING)
  (let-syntax-pattern ([compare (shared-syntax 'compare)]
                       [ofname (shared-syntax 'ofname)])
                      #'(define (compare)
                          (check-equal? (file->lines ofname) (file->lines COMPARE-FILE-STRING)))))

(define-macro (output-list-expr (COL-NAME FORMAT-SPEC) ...)
  (let-syntax-pattern ([(COL-ID ...) (prefix-ids "" #'(COL-NAME ...))]
                       [(CHIP-COL-ID ...) (prefix-ids chip-prefix "-" #'(COL-NAME ...))]
                       [output (shared-syntax 'output)]
                       [of (shared-syntax 'of)]
                       [eval-result (shared-syntax 'eval-result)]
                       [eval-thunk (shared-syntax 'eval-thunk)])
                      #'(begin
                          (define (output COL-ID ...)
                            (fprintf of (format "~a\n" (string-join (list (hdlprint COL-ID FORMAT-SPEC) ...) "|"
                                                                    #:before-first "|"
                                                                    #:after-last "|"))))
                          (define eval-result #f)
                          (define eval-thunk (λ () (list (CHIP-COL-ID) ...)))
                          (output COL-NAME ...))))

(define-macro (set-expr IN-BUS IN-VAL)
  (let-syntax-pattern ([CHIP-IN-BUS-ID-WRITE (prefix-id chip-prefix "-" (suffix-id #'IN-BUS "-write"))])
                      #'(CHIP-IN-BUS-ID-WRITE IN-VAL)))

(define-macro (eval-expr)
  (let-syntax-pattern ([eval-result (shared-syntax 'eval-result)]
                       [eval-thunk (shared-syntax 'eval-thunk)])
                      #'(set! eval-result (eval-thunk))))

(define-macro (output-expr)
  (let-syntax-pattern ([output (shared-syntax 'output)]
                       [eval-result (shared-syntax 'eval-result)])
                      #'(apply output eval-result)))

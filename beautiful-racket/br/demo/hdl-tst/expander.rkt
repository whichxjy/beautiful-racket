#lang br
(require (for-syntax br/syntax br/scope racket/string)
         "hdlprint.rkt" rackunit racket/file)
(provide #%top-interaction #%module-begin #%datum #%app (all-defined-out))


(define-for-syntax chip-prefix #f)


(define-macro (tst-program ARG ...)
  (let-shared-id (compare output-file)
                 #'(begin ARG ...
                          (close-output-port output-file)
                          (compare))))


(define-macro (load-expr CHIPFILE-STRING)
  (set! chip-prefix (string-replace (syntax->datum #'CHIPFILE-STRING) ".hdl" ""))
  (let-syntax-pattern ([CHIPFILE.RKT (format-string "~a.rkt" #'CHIPFILE-STRING)])
                      #'(require CHIPFILE.RKT)))


(define-macro (output-file-expr OUTPUT-FILE-STRING)
  (let-shared-id (output-file output-filename)
                 #'(begin
                     (define output-filename OUTPUT-FILE-STRING)
                     (define output-file (open-output-file output-filename #:exists 'replace)))))


(define-macro (compare-to-expr COMPARE-FILE-STRING)
  (let-shared-id (compare output-filename)
                 #'(define (compare)
                     (check-equal? (file->lines output-filename) (file->lines COMPARE-FILE-STRING)))))


(define-macro (output-list-expr (COL-NAME FORMAT-SPEC) ...)
  (let-shared-id (output output-file eval-result eval-thunk)
                 (let-syntax-pattern ([(COL-ID ...) (suffix-ids #'(COL-NAME ...))]
                                      [(CHIP-COL-ID ...) (prefix-ids chip-prefix "-" #'(COL-NAME ...))])
                                     #'(begin
                                         (define (output COL-ID ...)
                                           (fprintf output-file
                                                    (format "~a\n" (string-join (list (hdlprint COL-ID FORMAT-SPEC) ...) "|"
                                                                                #:before-first "|"
                                                                                #:after-last "|"))))
                                         (define eval-result #f)
                                         (define (eval-thunk) (list (CHIP-COL-ID) ...))
                                         (output COL-NAME ...)))))


(define-macro (set-expr IN-BUS IN-VAL)
  (let-syntax-pattern ([CHIP-IN-BUS-ID-WRITE (prefix-id chip-prefix "-" (suffix-id #'IN-BUS "-write"))])
                      #'(CHIP-IN-BUS-ID-WRITE IN-VAL)))


(define-macro (eval-expr)
  (let-shared-id (eval-result eval-thunk)
                 #'(set! eval-result (eval-thunk))))


(define-macro (output-expr)
  (let-shared-id (output eval-result)
                 #'(apply output eval-result)))

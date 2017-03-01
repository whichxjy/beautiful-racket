#lang br
(require "struct.rkt" "expr.rkt")
(provide b-rem b-print b-let b-input b-import b-export b-repl)

(define (b-rem val) (void))

(define (b-print . vals)
  (displayln (string-append* (map ~a vals))))

(define-macro (b-let ID VAL) #'(set! ID VAL))

(define-macro (b-input ID)
  #'(b-let ID (let* ([str (read-line)]
                     [num (string->number (string-trim str))])
                (or num str))))

(define-macro (b-import NAME) #'(void))

(define-macro (b-export NAME) #'(void))

(define-macro (b-repl . ARGS)
  (with-pattern ([STMTS (pattern-case-filter #'ARGS
                          [(b-expr . EXPR-ARGS)
                           #'(b-print (b-expr . EXPR-ARGS))]
                          [OTHER-STMT #'OTHER-STMT])])
    #'(begin . STMTS)))
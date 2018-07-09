#lang br/quicklang
(require "grammar.rkt" brag/support racket/stxparam)
(provide #%module-begin (all-defined-out))

(module+ reader
  (provide read-syntax))

(define-lex-abbrev reserved-terms
  (:or "def" "(" ")" ":" "," "return" "<" "if" "else"))
(define-lex-abbrev indent (:: (:+ "\n") (:* " ")))

(define prev-indent 0)
(define pending-dedents 0)

(define (lex ip)
  (define inner-lex
    (lexer
     [(eof) (cond
              [(> prev-indent 0)
               (set! pending-dedents prev-indent)
               (set! prev-indent 0)
               (lex input-port)]
              [else eof])]
     [(from/stop-before "#" "\n") (token 'COMMENT #:skip? #t)]
     [indent
      (match-let* ([(list _ spaces) (regexp-match #rx"^\n+( *)$" lexeme)]
                   [this-indent (/ (string-length spaces) 2)])
        (define tok
          (cond
            [(> (- this-indent prev-indent) 1) (error 'only-one-indent-please)]
            [(> this-indent prev-indent) (token 'INDENT)]
            [(< this-indent prev-indent)
             (set! pending-dedents (- prev-indent this-indent))
             (lex input-port)]
            [(= this-indent prev-indent) (token lexeme #:skip? #t)]))
        (set! prev-indent this-indent)
        tok)]
     [(:+ whitespace) (token lexeme #:skip? #t)]
     [reserved-terms lexeme]
     [(:+ (:- (:or alphabetic punctuation) reserved-terms))
      (token 'ID (string->symbol lexeme))]
     [(:+ (char-set "0123456789"))
      (token 'INTEGER (string->number lexeme))]))
  (cond
    [(equal? (peek-char ip) #\") (token 'STRING (read ip))]
    [(> pending-dedents 0)
     (set! pending-dedents (sub1 pending-dedents))
     (token 'DEDENT)]
    [else (inner-lex ip)]))

(define-macro top #'begin)

(define-macro-cases comparison
  [(_ LEFT "<" RIGHT) #'(< LEFT RIGHT)]
  [(_ OTHER) #'OTHER])

(define-macro (func-def VAR VARS STMT ...)
  #'(define (VAR . VARS)
      (let/cc return-cc
        (syntax-parameterize ([return (make-rename-transformer #'return-cc)])
          STMT ... (void)))))

(define-syntax-parameter return (λ (stx) (error 'not-parameterized)))

(define-macro func-app #'#%app)

(define-macro block #'begin)

(provide (rename-out [my-if if]))
(define-macro (my-if COND TBLOCK FBLOCK)
  #'(if COND (let () TBLOCK) (let () FBLOCK)))

(define (read-syntax src ip)
  (define parse-tree (parse (λ () (lex ip))))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module pyname pythonesque-demo
         PT))))
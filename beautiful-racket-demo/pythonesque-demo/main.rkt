#lang br/quicklang
(require "grammar.rkt" brag/support racket/pretty racket/stxparam)
(provide (except-out (all-from-out br/quicklang) for if print) (all-defined-out) pretty-print)

(module+ reader
  (provide read-syntax))

(define-lex-abbrev reserved-terms
  (:or "=" "def" "(" ")" ":" ","
       "return" "for" "in"
       "+" "-" "*" "/" "<" ">" "\""
       "if" "else" "print"))
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
     [reserved-terms (token lexeme (string->symbol lexeme))]
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
(define-macro (assignment ID EXPR)
  #'(define ID EXPR))

(define-macro-cases comparison
  [(_ ARG) #'ARG]
  [(_ LARG OP RARG) #'(OP LARG RARG)])

(define-macro sum #'comparison)
(define-macro product #'comparison)

(define-macro (func-def ID ID-ARGS STMT ...)
  #'(define (ID . ID-ARGS)
      (let/cc return-cc
        (syntax-parameterize ([return (make-rename-transformer #'return-cc)])
          STMT ... (void)))))

(define-syntax-parameter return (λ (stx) (error 'not-parameterized)))
(define-macro func-app #'#%app)

(provide (rename-out [my-for for]))
(define-macro (my-for ID EXPR . STMTS)
  #'(for ([ID (in-list EXPR)])
      . STMTS))

(define-macro block #'begin)

(provide (rename-out [my-if if]))
(define-macro-cases my-if
  [(_ COND TBLOCK) #'(when COND TRUE-BLOCK)]
  [(_ COND TBLOCK FBLOCK) #'(if COND (let () TBLOCK) (let () FBLOCK))])

(provide (rename-out [my-print print]))
(define-macro (my-print EXPR)
  #'(display EXPR))

(define (read-syntax src ip)
  (define parse-tree (parse (λ () (lex ip))))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module _ pythonesque-demo
         #;(pretty-print 'PT)
         PT))))
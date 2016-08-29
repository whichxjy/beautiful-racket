#lang br/quicklang
(require "bf-parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (tokenize port)))
  (define module-datum `(module bf-mod br/demo/bf/bf-expander
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)

(require parser-tools/lex brag/support)
(define (tokenize port)
  (define (next-token)
    (define our-lexer
      (lexer
       [(eof) eof]
       [(char-set "><-.,+[]") lexeme]
       [any-char (token 'COMMENT #:skip? #t)]))
    (our-lexer port))  
  next-token)

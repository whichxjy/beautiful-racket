#lang at-exp br/quicklang
(require "parser.rkt")

(define (read-syntax path port)
  (define parse-tree (parse path (tokenize port)))
  (define module-datum `(module bf-mod br/demo/jsonic/expander
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)

(require parser-tools/lex parser-tools/lex-sre brag/support)
(define (tokenize port)
  (define (next-token)
    (define our-lexer
      (lexer
       [(eof) eof]
       [(or (seq "//" (complement (seq any-string "\n" any-string)) "\n")) (next-token)]
       [(seq "@$") (token 'OPEN lexeme)]
       [(seq "$@") (token 'CLOSE lexeme)]
       [any-char (token 'CHAR lexeme)]))
    (our-lexer port))  
  next-token)

(define (test-tokenize str)
  (define ip (open-input-string str))
  (define token-producer (tokenize ip))
  (for/list ([token (in-producer token-producer eof)])
            token))

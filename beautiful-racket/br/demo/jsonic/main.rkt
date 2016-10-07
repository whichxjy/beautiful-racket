#lang at-exp br/quicklang
(require "parser.rkt")

(module+ reader
  (define (read-syntax path port)
    (define parse-tree (parse path (tokenize port)))
    (define module-datum `(module bf-mod br/demo/jsonic/expander
                            ,parse-tree))
    (datum->syntax #f module-datum))
  (provide read-syntax get-info))

(require parser-tools/lex parser-tools/lex-sre brag/support)
(define (tokenize port)
  (define (next-token)
    (define our-lexer
      (lexer
       [(eof) eof]
       ;; (char-complement "\n") means any char but "\n"
       ;; (complement "\n") means any whole string except "\n"
       [(seq "//" (* (char-complement "\n"))) (next-token)]
       ["@$" (token 'OPEN lexeme)]
       ["$@" (token 'CLOSE lexeme)]
       [any-char (token 'CHAR lexeme)]))
    (our-lexer port))  
  next-token)

(define (get-info . _)
  (λ (key default)
    (case key
      [(color-lexer)
       (dynamic-require 'br/demo/jsonic/drracket-lexer 'drracket-lexer (λ () #f))]
      [(drracket:indentation)
       (dynamic-require 'br/demo/jsonic/drracket-indenter 'drracket-indenter (λ () #f))
       #;(really-dynamic-require 'scribble/private/indentation 'determine-spaces)]
      [else default])))

(define (test-tokenize str)
  (define ip (open-input-string str))
  (define token-producer (tokenize ip))
  (for/list ([token (in-producer token-producer eof)])
            token))

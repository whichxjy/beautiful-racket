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
       [(seq (* "\n") (* whitespace) "//" any-string (* "\n")) (next-token)]
       [whitespace (next-token)]
       [(char-set ",:[]{}") lexeme]
       [(seq (* "-") (+ (or numeric "."))) (token 'NUMBER lexeme)] ;; Q: what is grammar for a JS number?
       [(seq "\"" (complement (seq any-string "\"" any-string)) "\"") (token 'STRING (string-trim lexeme "\""))]))
    (our-lexer port))  
  next-token)

(define (test-tokenize str)
  (define ip (open-input-string str))
  (define token-producer (tokenize ip))
  (for/list ([token (in-producer token-producer eof)])
            token))

(module+ main
  (test-tokenize @string-append{
 {"id": "file"
  // yeah baby
  }}))
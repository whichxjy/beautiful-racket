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
       [(or whitespace
            (seq "//" (complement (seq any-string "\n" any-string)) "\n")) (next-token)]
       [(char-set ",:[]{}@#") lexeme]
       [(seq (repetition 0 1 "-") (+ numeric) (repetition 0 1 (seq "." (* numeric))))
        (token 'NUMBER lexeme)] ;; Q: what is grammar for a JS number?
       [(seq "\"" (complement (seq any-string "\"" any-string)) "\"") (token 'STRING (string-trim lexeme "\""))]
       [any-char lexeme]))
    (our-lexer port))  
  next-token)

(define (test-tokenize str)
  (define ip (open-input-string str))
  (define token-producer (tokenize ip))
  (for/list ([token (in-producer token-producer eof)])
            token))

(test-tokenize #<<HERE
{"foo": @#(+ 4 2)#}
HERE
               )

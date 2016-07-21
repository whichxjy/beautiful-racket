#lang br/quicklang
(require parser-tools/lex brag/support)

(define (tokenize input-port)
  (define (next-token)
    (define get-token
      (lexer-src-pos
       [(char-set "><-.,+[]") lexeme]
       [(char-complement (char-set "><-.,+[]")) 
        (token 'OTHER #:skip? #t)]
       [(eof) eof]))
    (get-token input-port))  
  next-token)

(require "bf-parser.rkt")
(define (read-syntax source-path input-port)
  (define parse-tree (parse source-path (tokenize input-port)))
  (datum->syntax #f `(module bf-mod br/demo/bf/bf-expander
                       ,parse-tree)))
(provide read-syntax)

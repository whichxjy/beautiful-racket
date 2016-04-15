#lang br
(require parser-tools/lex ragg/support)
(define (tokenize input-port)
  (define (next-token)
    (define get-token
      (lexer
       [(char-set "><-.,+[]") lexeme]
       [(char-complement (char-set "><-.,+[]")) 
         (token 'OTHER #:skip? #t)]
       [(eof) eof]))
    (get-token input-port))  
  next-token)

(require "bf-parser.rkt")
(define (read-syntax source-path input-port)
  (define parse-tree (parse source-path (tokenize input-port)))
  (strip-context
    (inject-syntax ([#'PARSE-TREE parse-tree])
                  #'(module bf-mod br/bf/bf-expander
                      PARSE-TREE))))
(provide read-syntax)

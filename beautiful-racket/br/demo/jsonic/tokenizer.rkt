#lang br/quicklang
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
(provide tokenize)
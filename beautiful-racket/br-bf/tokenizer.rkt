#lang racket/base
(require parser-tools/lex (prefix-in : parser-tools/lex-sre) ragg/support)
(provide tokenize)

;; tokenizer prepares source for parser by
;; 1) identifying tokens, the smallest unit of information
;; 2) throwing away anything irrelevant (whitespace, comments)
;; tokenizer cooperates with the lexer, which is a fancy regular-expression processor

(define (tokenize ip)
  (define get-token
    (lexer
     [(char-set "><-.,+[]") lexeme]
     ;; todo: try adding support for line comments
     #;[(:: "#" (:* (complement "\n")) "\n") (token 'comment #:skip? #t)]
     [whitespace (token 'white #:skip? #t)]
     ;; treat other characters as comments
     [(char-range #\nul #\~) (token 'ascii #:skip? #t)]
     [(eof) eof]))
  
  (define (next-token) (get-token ip))
  
  next-token)

(module+ test
  (require rackunit)
  (define (test-tokenize str)
    (define ip (open-input-string str))
    (define token-producer (tokenize ip))
    (for/list ([token (in-producer token-producer eof)])
              token))
  
  (check-equal? (test-tokenize "+") (list "+")))

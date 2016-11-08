#lang racket/base
(require "support.rkt"
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(provide (all-from-out "support.rkt")
         (all-from-out parser-tools/lex)
         (all-from-out parser-tools/lex-sre)
         (all-defined-out))

(define (apply-tokenizer tokenize in)
    (define input-port (if (string? in)
                           (open-input-string in)
                           in))
    (define token-producer (tokenize input-port))
    (for/list ([token (in-producer token-producer eof)])
              token))
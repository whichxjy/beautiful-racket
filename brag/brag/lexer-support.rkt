#lang racket/base
(require "support.rkt"
         parser-tools/lex
         racket/string
         (prefix-in : parser-tools/lex-sre)
         (for-syntax racket/base))
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

(define (trim-delimiters left lexeme right)
  (string-trim (string-trim lexeme left #:right? #f) right #:left? #f))

(define-lex-trans delimited-by
  (λ(stx)
    (syntax-case stx ()
      [(_ OPEN CLOSE)
       #'(:seq OPEN (complement (:seq any-string CLOSE any-string)) CLOSE)])))
#lang br/quicklang
(require brag/support "grammar.rkt")
(provide (all-defined-out) #%module-begin)

(module+ reader
  (provide read-syntax))

(define-lex-abbrev reserved-toks
  (:or "fun" "(" ")" "=" "+" ","))

(define lex
  (lexer
   [whitespace (lex input-port)]
   [reserved-toks lexeme]
   [alphabetic (token 'ID (string->symbol lexeme))]
   [(:+ (char-set "0123456789")) (token 'INT (string->number lexeme))]))

(define-macro top #'begin)

(define-macro (func-def ID ARGIDS EXPR)
  #'(define ID (λ ARGIDS EXPR)))

(define-macro-cases expr
  [(_ LEFT "+" RIGHT) #'(+ LEFT RIGHT)]
  [(_ OTHER) #'OTHER])

(define-macro (func-app ID ARG ...)
  #'(ID ARG ...))

(define (read-syntax src ip)
  (define token-thunk (λ () (lex ip)))
  (define parse-tree (parse token-thunk))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module mod-name algebra-demo
         PT))))
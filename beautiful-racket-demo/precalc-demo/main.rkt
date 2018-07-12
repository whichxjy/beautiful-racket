#lang br/quicklang
(require brag/support "grammar.rkt")
(provide (all-defined-out) #%module-begin)

(module+ reader
  (provide read-syntax))

(define-lex-abbrev reserved-toks
  (:or "fun" "(" ")" "=" "+" "*" "/" "-" ","))

(define tokenize
  (lexer
   [(:or (from/to "//" "\n") (from/to "/*" "*/")) (token 'COMMENT #:skip? #t)]
   [whitespace (tokenize input-port)]
   [reserved-toks lexeme]
   [alphabetic (token 'ID (string->symbol lexeme))]
   [(:+ (char-set "0123456789")) (token 'INT (string->number lexeme))]))

(define-macro top #'begin)

(define-macro (func-def VAR VARS EXPR)
  #'(define (VAR . VARS) EXPR))

(define-macro-cases sum
  [(_ LEFT "+" RIGHT) #'(+ LEFT RIGHT)]
  [(_ LEFT "-" RIGHT) #'(- LEFT RIGHT)]
  [(_ OTHER) #'OTHER])

(define-macro-cases product
  [(_ LEFT OP-STR RIGHT)
   (with-syntax ([OP (string->symbol (syntax->datum #'OP-STR))])
     #'(OP LEFT RIGHT))]
  [(_ OTHER) #'OTHER])

(define-macro func-app #'#%app)

(define (read-syntax src ip)
  (define parse-tree (parse (λ () (tokenize ip))))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module mod-name precalc-demo
         PT))))
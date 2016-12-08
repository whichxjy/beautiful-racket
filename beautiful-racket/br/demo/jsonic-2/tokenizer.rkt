#lang br/quicklang
(require brag/support)

(module+ test
  (require rackunit))

(define (token? x)
  (or (eof-object? x) (string? x) (token-struct? x)))

(module+ test
  (check-true (token? eof))
  (check-true (token? "a string"))
  (check-true (token? (token 'A-TOKEN-STRUCT "hi")))
  (check-false (token? 42)))

(define/contract (tokenize port)
  (input-port? . -> . (-> token?))
  (port-count-lines! port)
  (define/contract (next-token)
    (-> token?)
    (define our-lexer
      (lexer
       [(eof) eof]
       [(from/to "//" "\n") (next-token)]
       [(from/to "@$" "$@")
        (token 'SEXP-TOK (trim-ends "@$" lexeme "$@")
               #:line (line lexeme-start)
               #:column (+ (col lexeme-start) 2)
               #:position (+ (pos lexeme-start) 2)
               #:span (- (pos lexeme-end)
                         (pos lexeme-start) 4))]
       [any-char (token 'CHAR-TOK lexeme
                        #:line (line lexeme-start)
                        #:column (col lexeme-start)
                        #:position (pos lexeme-start)
                        #:span (- (pos lexeme-end)
                                  (pos lexeme-start)))]))
    (our-lexer port))  
  next-token)
(provide tokenize)


(module+ test
  (check-equal? (apply-tokenizer tokenize "// comment\n") empty)
  (check-equal?
   (apply-tokenizer tokenize "@$ (+ 6 7) $@")
   (list (token 'SEXP-TOK " (+ 6 7) "
                #:line 11
                #:column 2
                #:position 3
                #:span 9)))
  (check-equal?
   (apply-tokenizer tokenize "hi")
   (list (token 'CHAR-TOK "h"
                #:line 1
                #:column 0
                #:position 1
                #:span 1)
         (token 'CHAR-TOK "i"
                #:line 1
                #:column 1
                #:position 2
                #:span 1))))
#lang br
(require "lexer.rkt" brag/support)
(provide color-basic)

(define (color-basic port)
  (define srcloc-tok (basic-lexer port))
  (match srcloc-tok
    [(? eof-object?) (values srcloc-tok 'eof #f #f #f)]
    [else ; reverse-engineer with `match-define`
     (match-define (srcloc-token (token-struct type val _ _ _ _ _)
                                 (srcloc _ _ _ pos span)) srcloc-tok)
     (define (color cat [paren #f])
       (values (or val "") cat paren pos (+ pos span)))
     (match type
       ['STRING (color 'string)]
       ['REM (color 'comment)]
       [else (match val
               [(? number?) (color 'constant)]
               [(? symbol?) (color 'symbol)]
               ["(" (color 'parenthesis '|(|)]
               [")" (color 'parenthesis '|)|)]
               [else (color 'no-color)])])]))
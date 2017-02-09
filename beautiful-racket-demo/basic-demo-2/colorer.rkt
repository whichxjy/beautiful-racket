#lang br
(require "lexer.rkt" brag/support)
(provide basic-colorer)

(define (basic-colorer port)
  (define (handle-lexer-error exn)
    (define exn-srclocs (exn:fail:read-srclocs exn))
    (srcloc-token (token 'ERROR) (car exn-srclocs)))
  (define srcloc-tok (with-handlers ([exn:fail:read? handle-lexer-error])
                       (basic-lexer port)))
  (match srcloc-tok
    [(? eof-object?) (values srcloc-tok 'eof #f #f #f)]
    [else
     (match-define (srcloc-token (token-struct type val _ _ _ _ _)
                                 (srcloc _ _ _ pos span)) srcloc-tok)
     (match-define (list cat paren)
       (match type
         ['STRING '(string #f)]
         ['REM '(comment #f)]
         ['ERROR '(error #f)]
         [else (match val
                 [(? number?)'(constant #f)]
                 [(? symbol?) '(symbol #f)]
                 ["(" '(parenthesis |(|)]
                 [")" '(parenthesis |)|)]
                 [else '(no-color #f)])]))
     (values (or val "") cat paren pos (+ pos span))]))
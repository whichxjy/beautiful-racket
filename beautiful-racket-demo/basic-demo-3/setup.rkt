#lang br
(require "parser.rkt" "tokenizer.rkt")
(provide basic-output-port do-setup!)

(define basic-output-port
  (make-parameter (open-output-nowhere)))

(define (do-setup! [where #f])
  (basic-output-port (current-output-port))
  #;(current-read-interaction read-one-line))

(define repl-parser (make-rule-parser b-repl))

(define (read-one-line path port)
  (define one-line (read-line port))
  (if (eof-object? one-line)
      eof
      (repl-parser
       (make-tokenizer (open-input-string one-line)))))



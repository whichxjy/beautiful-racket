#lang br
(require "parser.rkt" "tokenizer.rkt")
(provide basic-output-port configure-this!)

(define basic-output-port
  (make-parameter (open-output-nowhere)))

(define repl-parser (make-rule-parser b-repl))

(define (configure-this!)
  (basic-output-port (current-output-port))
  
  (define (read-one-line path port)
    (define one-line (read-line port))
    (if (eof-object? one-line)
        eof
        (repl-parser
         (make-tokenizer (open-input-string one-line)))))
  (current-read-interaction read-one-line))



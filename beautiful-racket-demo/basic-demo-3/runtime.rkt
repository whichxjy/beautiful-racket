#lang br
(require "parser.rkt" "tokenizer.rkt")
(provide current-basic-port configure-repl!)

(define current-basic-port (make-parameter #f))

(define (configure-repl!)
  (define statement-parser (make-rule-parser b-statement))
  (define (read-one-line path port)
    (define one-line (read-line port))
    (if (eof-object? one-line)
        eof
        (statement-parser (make-tokenizer (open-input-string one-line)))))
  (current-read-interaction read-one-line))



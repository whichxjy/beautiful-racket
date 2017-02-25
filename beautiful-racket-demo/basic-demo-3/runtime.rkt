#lang br
(require "parser.rkt" "tokenizer.rkt")
(provide basic-output-port configure-this!)

(define basic-output-port (make-parameter (open-output-nowhere)))

(define (configure-this!)
  (basic-output-port (current-output-port))
  
  (define statement-parser (make-rule-parser b-statement))
  (define (read-one-line path port)
    (define one-line (read-line port))
    (if (eof-object? one-line)
        eof
        (statement-parser (make-tokenizer (open-input-string one-line)))))
  (current-read-interaction read-one-line))



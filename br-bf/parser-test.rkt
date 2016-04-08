#lang racket
(require "tokenizer.rkt" "parser.rkt" ragg/support)

(syntax->datum (parse (tokenize (open-input-string "[+-]>"))))
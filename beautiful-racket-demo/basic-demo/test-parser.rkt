#lang br
(require "tokenizer.rkt" "parser.rkt" brag/support)

(define str #<<here
10 rem print
20 end

here
  )

(parse-tree (apply-tokenizer tokenize str))
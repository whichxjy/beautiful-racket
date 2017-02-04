#lang br
(require "tokenizer.rkt" "parser.rkt" brag/support)

(define str #<<here
10 rem print
20 end

here
  )

(parse-to-datum (apply-tokenizer-maker make-tokenizer str))
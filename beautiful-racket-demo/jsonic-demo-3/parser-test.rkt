#lang br
(require "parser.rkt" "tokenizer.rkt" brag/support rackunit)

(check-equal?
 (parse-tree
  (apply-tokenizer-maker make-tokenizer "// line commment\n"))
 '(jsonic-program))
(check-equal?
 (parse-tree
  (apply-tokenizer-maker make-tokenizer "@$ 42 $@"))
 '(jsonic-program (s-exp " 42 ")))
(check-equal?
 (parse-tree
  (apply-tokenizer-maker make-tokenizer "hi"))
 '(jsonic-program
   (json-char "h")
   (json-char "i")))
(check-equal?
 (parse-tree
  (apply-tokenizer-maker make-tokenizer
                         "hi\n// comment\n@$ 42 $@"))
 '(jsonic-program
   (json-char "h")
   (json-char "i")
   (json-char "\n")
   (s-exp " 42 ")))
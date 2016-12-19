#lang racket/base
(require brag/examples/whitespace
         brag/support
         rackunit)

(check-equal?
 (parse-tree "\ty\n x\tz")
 '(start (tab "\t") (letter "y") (newline "\n") (space " ") (letter "x") (tab "\t") (letter "z")))

(check-equal?
 (parse-tree "\t\n \t")
 '(start (tab "\t") (newline "\n") (space " ") (tab "\t")))

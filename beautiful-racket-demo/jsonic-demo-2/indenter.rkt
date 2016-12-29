#lang br
(require br/indent racket/gui/base racket/contract)
(provide indent-jsonic)

(define indent-width 2)
(define (left-bracket? c) (member c (list #\{ #\[)))
(define (right-bracket? c) (member c (list #\} #\])))

(define/contract (indent-jsonic tbox [posn 0])
  ((is-a?/c text%) exact-nonnegative-integer?  . -> .
                   (or/c exact-nonnegative-integer? #f))
  (define prev-line (previous-line tbox posn))
  (define current-line (line tbox posn))
  (define prev-indent (or (line-indent tbox prev-line) 0))
  (define current-indent
    (cond
      [(left-bracket?
        (line-first-visible-char tbox prev-line))
       (+ prev-indent indent-width)]
      [(right-bracket?
        (line-first-visible-char tbox current-line))
       (- prev-indent indent-width)]
      [else prev-indent]))
  (and (exact-positive-integer? current-indent) current-indent))

(module+ test
  (require rackunit)
  (define test-str #<<HERE
#lang jsonic
{
"value",
"string":
[
{
"array": @$(range 5)$@,
"object": @$(hash 'k1 "valstring")$@
}
]
// "bar"
}
HERE
    )
  (check-equal?
   (string-indents (apply-indenter indent-jsonic test-str))
   '(#f #f 2 2 2 4 6 6 4 2 2 #f)))
  

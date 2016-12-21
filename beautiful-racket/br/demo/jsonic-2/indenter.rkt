#lang br
(require br/indent racket/gui/base)
(provide indent-jsonic)

(define indent-width 2)

(define (left-bracket? c) (member c '(#\{ #\[)))
(define (right-bracket? c) (member c '(#\} #\])))

;; if this line begins with } or ], outdent.
;; if last line begins with { or [, indent.
;; otherwise use previous indent
(define/contract (indent-jsonic textbox [tbpos 0])
  ((is-a?/c text%) exact-nonnegative-integer?  . -> .
                   (or/c exact-nonnegative-integer? #f))
  (define this-line (line textbox tbpos))
  (define prev-line (previous-line textbox tbpos))
  (define prev-indent (or (line-indent textbox prev-line) 0))
  (define this-indent
    (cond
      [(left-bracket? (char textbox (line-start-visible textbox prev-line)))
       (+ prev-indent indent-width)]
      [(right-bracket? (char textbox (line-start-visible textbox this-line)))
       (- prev-indent indent-width)]
      [else prev-indent]))
  (and (exact-positive-integer? this-indent) this-indent))

(module+ test
  (require rackunit)
  (define test-str #<<here
#lang br/demo/jsonic
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
here
    )
  (display (apply-indenter indent-jsonic test-str)))

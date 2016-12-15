#lang br
(require br/indent)
(provide indent-jsonic)

(define indent-width 2)

(define (left-bracket? c) (member c '(#\{ #\[)))
(define (right-bracket? c) (member c '(#\} #\])))

;; if this line begins with } or ], outdent.
;; if last line begins with { or [, indent.
;; otherwise use previous indent
(define (indent-jsonic textbox [pos 0])
  (define this-line (line textbox pos))
  (define prev-line (previous-line textbox pos))
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
"string": @$(string-append "foo" "bar")$@,
{
"array": @$(range 5)$@,
"object": @$(hash "k1" "valstring" (format "~a" 42) (hash "k1" (range 10) "k2" 42))$@
}
// "bar" :
}
here
    )
  (check-equal? (string-indents (apply-indenter indent-jsonic test-str))
                (map (λ(x) (* x indent-width)) '(0 0 1 1 2 2 1 1 0))))

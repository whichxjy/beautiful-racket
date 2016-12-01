#lang at-exp br
(require br/indent)
(provide indenter)

(define indent-width 2)

(define (indenter tb [this-pos 0])
  (define this-line (line tb this-pos))
  (define prev-line (previous-line tb this-pos))
  (define prev-indent (or (line-indent tb prev-line) 0))
  (define this-indent
    (cond
      ;; if this line begins with }, outdent.
      [((char tb (line-start-visible tb this-line)) . char=? . #\})
       (- prev-indent indent-width)]
      ;; if last line begins with {, indent.
      [((char tb (line-start-visible tb prev-line)) . char=? . #\{)
       (+ prev-indent indent-width)]
      ;; otherwise use previous indent
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
  (displayln (test-indenter indenter test-str))
  (check-equal? (str->indents (test-indenter indenter test-str))
                (map (λ(x) (* x indent-width)) '(0 0 1 1 2 2 1 1 0))))
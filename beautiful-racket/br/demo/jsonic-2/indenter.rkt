#lang at-exp br
(require br/indent)
(provide indent-jsonic)

(define indent-width 2)

(define (left-bracket? c)
   (and c (or (char=? c #\{) (char=? c #\[))))

(define (right-bracket? c)
   (and c (or (char=? c #\}) (char=? c #\]))))

(define (indent-jsonic tb [this-pos 0])
  (define this-line (line tb this-pos))
  (define prev-line (previous-line tb this-pos))
  (define prev-indent (or (line-indent tb prev-line) 0))
  (define this-indent
    (cond
      ;; if this line begins with }, outdent.
      [(right-bracket? (char tb (line-start-visible tb this-line)))
       (- prev-indent indent-width)]
      ;; if last line begins with {, indent.
      [(left-bracket? (char tb (line-start-visible tb prev-line)))
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
  (check-equal? (str->indents (test-indenter indent-jsonic test-str))
                (map (λ(x) (* x indent-width)) '(0 0 1 1 2 2 1 1 0))))

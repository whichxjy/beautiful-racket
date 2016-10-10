#lang at-exp br
(require br/indent sugar/debug)
(provide indenter)
(define indent-width 2)

(define (indenter text [this-pos 0])
  ;; if line begins with }:
  ;; outdent to the matching {
  ;; indent to match the previous line
  (define this-line (line text this-pos))
  (define this-line-end (line-end text this-line))
  (define open-braces
    (- (count-char text #\{ 0 this-line-end)
       (count-char text #\} 0 this-line-end)))
  (and (positive? open-braces)
       (* indent-width
          (if ((char text (line-start-visible text this-line)) . char=? . #\{)
              (sub1 open-braces)
              open-braces))))

#|
  (define prev-line (previous-line text this-line))
  (define prev-indent (line-indent text prev-line))
  (cond
    [((char text (line-start text this-line)) . char=? . #\})
     (and prev-indent (- prev-indent indent-width))]
    [((char text (line-start text prev-line)) . char=? . #\{)
     (+ (or prev-indent 0) indent-width)]
    [else prev-indent]))
|#

(module+ test
  (require rackunit)
  (define test-str @string-append|{
#lang br/demo/jsonic
{
"string": @$(string-append "foo" "bar")$@,
{
"array": @$(range 5)$@,
"object": @$(hash "k1" "valstring" (format "~a" 42) (hash "k1" (range 10) "k2" 42))$@
}
// "bar" :
}}|)
  (displayln test-str)
  (define indented-str (test-indenter indenter test-str))
  (displayln indented-str)
  (define indented-str2 (test-indenter indenter indented-str))
  (displayln indented-str2)
  (define indented-str3 (test-indenter indenter indented-str2))
  (displayln indented-str3))
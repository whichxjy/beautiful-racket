#lang br
(require br/indent)
(provide indenter)
(define indent-width 2)

(define (indenter text this-pos)
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
          (if ((char text (line-start text this-line)) . char=? . #\{)
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
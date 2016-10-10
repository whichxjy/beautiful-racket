#lang br
(require br/indent)
(provide indenter)
(define indent-width 2)

(define (indenter text start-pos)
  ;; if line begins with }:
  ;; outdent to the matching {
  ;; indent to match the previous line
  (define the-line (line text start-pos))
  (define line-last-pos (line-end text the-line))
  (define open-braces
    (- (count-char text #\{ 0 line-last-pos)
       (count-char text #\} 0 line-last-pos)))
  (define line-first-pos (line-start text the-line))
  (define first-char (char text line-first-pos))
  (and (positive? open-braces)
       (* indent-width
          (if (first-char . char=? . #\{)
              (sub1 open-braces)
              open-braces))))

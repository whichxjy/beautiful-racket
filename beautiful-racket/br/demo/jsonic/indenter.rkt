#lang br
(provide indenter)

(define open-braces #f)
(define indent-width 2)

(define (indenter drr-editor start-pos)
  (when (zero? start-pos) (set! open-braces 0))
  (define first-pos-in-this-line
    (for*/first ([pos (in-naturals start-pos)]
                 [c (in-value (send drr-editor get-character pos))]
                 #:when (not (char-blank? c)))
                pos))
  (define last-pos-in-this-line
    (send drr-editor find-newline 'forward first-pos-in-this-line))
  (set! open-braces
        (+ open-braces
           (for/sum ([pos (in-range first-pos-in-this-line last-pos-in-this-line)])
                    (case (send drr-editor get-character pos)
                      [(#\{) 1]
                      [(#\}) -1]
                      [else 0]))))
  (define first-char
    (send drr-editor get-character first-pos-in-this-line))
  (and (positive? open-braces)
       (* indent-width
          (if (first-char . char=? . #\{)
              (sub1 open-braces)
              open-braces))))

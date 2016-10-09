#lang br
(provide indenter)

(define indent-width 2)

(define (indenter drr-editor start-pos)
  (define line-first-pos
    (for*/first ([pos (in-naturals start-pos)]
                 [c (in-value (send drr-editor get-character pos))]
                 #:when (not (char-blank? c)))
                pos))
  (define line-last-pos
    (send drr-editor find-newline 'forward line-first-pos))
  (define open-braces
    (for/sum ([pos (in-range 0 line-last-pos)])
             (case (send drr-editor get-character pos)
               [(#\{) 1]
               [(#\}) -1]
               [else 0])))
  (define first-char
    (send drr-editor get-character line-first-pos))
  (and (positive? open-braces)
       (* indent-width
          (if (first-char . char=? . #\{)
              (sub1 open-braces)
              open-braces))))

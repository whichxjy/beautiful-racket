#lang br
(require racket/class describe)
(provide drracket-indenter)

(define open-braces #f)
(define indent-width 2)

(define (drracket-indenter txt start-pos)
  (define fresh-indent? (zero? start-pos))
  (when fresh-indent? (set! open-braces 0))
  (define first-pos-in-this-line
    (for*/first ([pos (in-naturals start-pos)]
                 [c (in-value (send txt get-character pos))]
                 #:when (not (char-blank? c)))
                pos))
  (define last-pos-in-this-line
    (send txt find-newline 'forward first-pos-in-this-line))
  (set! open-braces
        (+ open-braces
           (for/sum ([pos (in-range first-pos-in-this-line last-pos-in-this-line)])
                    (case (send txt get-character pos)
                      [(#\{) 1]
                      [(#\}) -1]
                      [else 0]))))
  (and (positive? open-braces)
       (* indent-width
          (if ((send txt get-character first-pos-in-this-line) . char=? . #\{)
              (sub1 open-braces)
              open-braces))))

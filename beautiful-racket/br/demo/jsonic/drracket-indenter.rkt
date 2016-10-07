#lang br
(require racket/class describe)
(provide drracket-indenter)

(define (drracket-indenter txt pos)
  (define i (char->integer (send txt get-character pos)))
  (if (zero? i)
      #f
      i))
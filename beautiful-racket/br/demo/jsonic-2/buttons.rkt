#lang br
(require br/drracket)
         
(define (button-func drr-window)
  (define expr-string "@$  $@")
  (define editor (send drr-window get-definitions-text))
  (send editor begin-edit-sequence)
  (send editor insert expr-string)
  (send editor end-edit-sequence)
  (define pos (send editor get-end-position))
  (send editor set-position (- pos (/ (string-length expr-string) 2))))

(define button-list
  (list (make-drracket-button "Insert expression" button-func)))
(provide button-list)

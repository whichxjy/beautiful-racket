#lang br
(require racket/draw)
(provide make-jsonic-buttons)

(define make-jsonic-buttons
  (list (let ([label "Insert expression"]
              [bitmap (make-object bitmap% 16 16)]
              [callback (λ (drr-frame)
                          (define drr-editor (send drr-frame get-definitions-text))
                          (define block-string "@$  $@")
                          (send drr-editor begin-edit-sequence)
                          (send drr-editor insert block-string)
                          (send drr-editor end-edit-sequence)
                          (define pos (send drr-editor get-end-position))
                          (send drr-editor set-position (- pos (/ (string-length block-string) 2))))]
              [number 98])
          (list label bitmap callback number))))
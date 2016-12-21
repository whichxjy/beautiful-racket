#lang br
(require racket/draw)
(provide make-button)

(define label "Insert expression")
(define bitmap (make-object bitmap% 16 16))
(define (callback drr-frame)
  (define drr-editor (send drr-frame get-definitions-text))
  (define block-string "@$  $@")
  (send drr-editor begin-edit-sequence)
  (send drr-editor insert block-string)
  (send drr-editor end-edit-sequence)
  (define pos (send drr-editor get-end-position))
  (send drr-editor set-position (- pos (/ (string-length block-string) 2))))
(define number 98)

(define make-button
  (list (list label bitmap callback number)))
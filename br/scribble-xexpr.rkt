#lang at-exp br
(require scribble/manual scribble/core txexpr)
(provide scribble->xexpr)

(define (style->attrs s style-accessor)
  (let* ([style-datum (style-accessor s)])
    (if (style? style-datum) ; either style struct, or simple symbol name
        `((style ,(~a (style-name style-datum)))
          (properties ,(string-join (map ~a (style-properties style-datum)) " ")))
        `((style ,(~a style-datum))))))


;; Unfortunately there seems to be no generic way of fetching the style & elements from a Scribble structure
;; the specific struct accessors must be used.
(define #'structure->txexpr
  (λ(stx)
    (syntax-match stx
                  [#'(_ structure-name elem-name id)
                   (syntax-let ([#'structure-name-elem-name (format-id stx "~a-~a" #'structure-name #'elem-name)]
                                [#'structure-name-style (format-id stx "~a-style" #'structure-name)])
                               #'(let* ([elem-raw (structure-name-elem-name id)]
                                        [elems (map scribble->xexpr (if (list? elem-raw)
                                                                        (flatten elem-raw)
                                                                        (list elem-raw)))])
(list* 'structure-name (style->attrs id structure-name-style) elems)))])))

(define (scribble->xexpr s)
  (cond
    [(nested-flow? s) (structure->txexpr nested-flow blocks s)]
    [(paragraph? s) (structure->txexpr paragraph content s)]
    [(element? s) (structure->txexpr element content s)]
    [else s]))

(module+ test
  (require rackunit)
  (define-simple-check (check-sx? s)
    (check-true (txexpr? (scribble->xexpr s))))
  (check-sx? @racketblock[(list +)])
  (check-sx? @racket[(list +)])
  (check-sx? @code{(list +)}))

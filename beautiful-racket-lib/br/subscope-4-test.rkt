#lang br
(require (for-syntax ) racket/splicing racket/function)

(define (->syntax x)
  (if (syntax? x) x (datum->syntax #f x)))

(define (context stx)
  (hash-ref (syntax-debug-info stx) 'context))

(define #'(define-scope _color . _parents)
  (with-syntax ([color-scope (suffix-id  #'_color "-scope")]
                [add-color (prefix-id "add-" #'_color)]
                [color? (suffix-id #'_color "?")]
                [color:define (suffix-id #'_color ":define")]
                [color-binding-id (suffix-id #'_color "-binding-id")]
                [color-binding-form (suffix-id  #'_color "-binding-form")]
                [color-string (format-string "~a" #'_color)])
    #'(begin
        (define color-scope (procedure-rename (make-syntax-introducer #t) 'color-scope))
        (define (add-color x) ((procedure-rename (curryr color-scope 'add) 'add-color) (->syntax x)))
        (define (color? x)
          (and
           (member (car (context (add-color (datum->syntax #f '_))))
                   (context (->syntax x)))
           #t))
        (define (color-binding-form x) (syntax-local-introduce (add-color x)))
        (define #'(color:define _id-in _expr)
          (inject-syntax* ([#'color:id (shared-syntax (prefix-id color-string ":" #'_id-in))]
                           [#'color-binding-id (syntax-shift-phase-level #'(color-binding-form #'_id-in) 1)]
                           [#'color-id (syntax-shift-phase-level #'(add-color color-binding-id) 1)])
                          #'(begin
                              (define color-binding-id _expr)
                              (define-syntax color:id
                                (syntax-id-rules ()
                                  [_ color-id]))))))))

(define-scope blue)

#;(blue:define x 42)

(require rackunit)
(check-true (blue? (add-blue #'x)))
(check-false (blue? #'x))




#|
(define (double-x)
  (with-blue-identifiers (x)
                         (set! x (+ x x))))

(define (display-x)
  (with-blue-identifiers (x)
                         (displayln x)))

(blue:define x 42)

blue:x

(double-x)

(display-x)
|#
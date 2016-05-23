#lang br
(require (for-syntax br/syntax racket/function) racket/splicing)

(define-for-syntax (->syntax x)
  (if (syntax? x) x (datum->syntax #f x)))

(define (context stx)
  (hash-ref (syntax-debug-info stx) 'context))

(define #'(define-scope _color . _parents)
  (let ([msi (make-syntax-introducer #t)])
    (with-syntax ([color-scope (suffix-id  #'_color "-scope")]
                  [add-color (prefix-id "add-" #'_color)]
                  [p1-add-color (prefix-id "p1-add-" #'_color)]
                  [color? (suffix-id #'_color "?")]
                  [color:define (suffix-id #'_color ":define")]
                  [msi msi])
      #'(begin
          (begin-for-syntax
            (define color-scope (begin (displayln (gensym)) (procedure-rename msi 'color-scope)))
            (define (add-color x) ((procedure-rename (curryr color-scope 'add) 'add-color) (->syntax x))))
          (define #'(color? _x)
            (with-syntax ([p1-add-color add-color])
              #'(and (member (report (car (context (p1-add-color (datum->syntax #f '_))))) (report (context #'_x))) #t)))
          (define #'(color:define _id-in _expr)
            (with-syntax ([colored-binding-id (add-color #'_id-in)]
                          [color:id (prefix-id #'_color ":" #'_id-in)])
              #'(begin
                  (define colored-binding-id _expr)
                  (define-syntax color:id
                    (syntax-id-rules ()
                      [_ colored-binding-id])))))))))

(require rackunit)

(define-scope blue)

(blue:define x 42)

(check-equal? blue:x 42)


(context #'blue:x)
(blue? blue:x)

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
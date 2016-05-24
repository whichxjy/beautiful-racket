#lang br
(require (for-syntax br/syntax))


(define (context stx)
  (hash-ref (syntax-debug-info stx) 'context))

(define blue-scope (begin (displayln (gensym)) (procedure-rename (make-syntax-introducer #t) 'blue-scope)))
(define (add-blue x) ((procedure-rename (λ(arg) (blue-scope arg 'add)) 'add-blue) x))
(define-for-syntax my-blue (syntax-shift-phase-level #'add-blue -1))
(define (blue? _x)
            (and (member (report (car (context (add-blue (datum->syntax #f '_))))) (report (context #'_x))) #t))
(define #'(blue:define _id-in _expr)
  (with-syntax* ([blue-binding-id (syntax-local-eval #`(my-blue #,#'_id-in))]
                 [blue:id (prefix-id "blue" ":" #'_id-in)])
    #'(begin
        (define blue-binding-id _expr)
        (define-syntax blue:id
          (syntax-id-rules ()
            [_ blue-binding-id])))))



(require rackunit)

(blue:define x 42)

#|
(check-equal? blue:x 42)

(blue? blue:x)
|#
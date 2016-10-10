#lang racket/base
(require racket/class racket/gui/base racket/list)
(provide (all-defined-out))

(module+ test
  (require racket/gui/base rackunit)
  (define t (new text%))
  (define result (send t insert-port (open-input-string "foo\n ar\n  m"))))

(define indent-width 2)

(define (char text pos)
  (and pos (send text get-character pos)))

(module+ test
  (check-equal? (char t 0) #\f)
  (check-equal? (char t 2) #\o)
  (check-equal? (char t 3) #\newline)
  (check-equal? (char t 10) #\m)
  (check-equal? (char t 11) #\nul))

(define (line text pos)
  (send text position-line pos))

(module+ test
  (check-equal? (line t 0) 0)
  (check-equal? (line t 2) 0)
  (check-equal? (line t 5) 1)
  (check-equal? (line t 10) 2)
  (check-equal? (line t 11) 2))

(define (line-chars text line)
  (and
   (valid-line? text line)
   (for/list ([pos (in-range (line-start text line) (add1 (line-end text line)))])
             (char text pos))))

(module+ test
  (check-equal? (line-chars t 0) '(#\f #\o #\o #\newline))
  (check-equal? (line-chars t 1) '(#\space #\a #\r #\newline))
  (check-equal? (line-chars t 2) '(#\space #\space #\m #\nul))
  (check-equal? (line-chars t 3) #f))


(define (previous-line text pos)
  (define this-line (line text pos))
  (and (this-line . > . 0) (sub1 this-line)))

(module+ test
  (check-equal? (previous-line t 0) #f)
  (check-equal? (previous-line t 2) #f)
  (check-equal? (previous-line t 5) 0)
  (check-equal? (previous-line t 10) 1)
  (check-equal? (previous-line t 11) 1))

(define (next-line text pos)
  (define last (send text last-line))
  (define this-line (line text pos))
  (and (this-line . < . last) (add1 this-line)))

(module+ test
  (check-equal? (next-line t 0) 1)
  (check-equal? (next-line t 2) 1)
  (check-equal? (next-line t 5) 2)
  (check-equal? (next-line t 10) #f)
  (check-equal? (next-line t 11) #f))

(define (valid-line? text line)
  (and line (<= 0 line (send text last-line))))

(define (line-start text line)
  (and (valid-line? text line)
       (send text line-start-position line)))

(module+ test
  (check-equal? (line-start t 0) 0)
  (check-equal? (line-start t 1) 4)
  (check-equal? (line-start t 2) 8)
  (check-equal? (line-start t 3) #f))
  
(define (line-end text line)
  (and (valid-line? text line)
       (send text line-end-position line)))

(module+ test
  (check-equal? (line-end t 0) 3)
  (check-equal? (line-end t 1) 7)
  (check-equal? (line-end t 2) 11)
  (check-equal? (line-end t 3) #f))

(define (first-visible-char-pos text start end)
  ;; private
  (for*/first ([pos (in-range start end (if (start . > . end) -1 1))]
               [c (in-value (char text pos))]
               #:when (not (char-blank? c)))
              pos))

(define (line-start-visible text line)
  (define start (line-start text line))
  (define end (line-end text line))
  (and start end (first-visible-char-pos text start end)))

(module+ test
  (check-equal? (line-start-visible t 0) 0)
  (check-equal? (line-start-visible t 1) 5)
  (check-equal? (line-start-visible t 2) 10)
  (check-equal? (line-start-visible t 3) #f))

(define (line-end-visible text line)
  (define start+1 (line-end text line)) ; start before newline
  (define end+1 (line-start text line))
  (and start+1 end+1 (first-visible-char-pos text (sub1 start+1) (sub1 end+1))))

(module+ test
  (check-equal? (line-end-visible t 0) 2)
  (check-equal? (line-end-visible t 1) 6)
  (check-equal? (line-end-visible t 2) 10)
  (check-equal? (line-end-visible t 3) #f))

(define (line-indent text line)
  (and (valid-line? text line)
       (let ([lsv (line-start-visible text line)])
         (and lsv ; could be #f
              (- (line-start-visible text line) (line-start text line))))))

(module+ test
  (check-equal? (line-indent t 0) 0)
  (check-equal? (line-indent t 1) 1)
  (check-equal? (line-indent t 2) 2)
  (check-equal? (line-indent t 3) #f))

(define (count-char text c [start 0] [end (send text last-position)])
  (for*/sum ([pos (in-range start (add1 end))])
            (if ((char text pos) . char=? . c)
                1
                0)))

(module+ test
  (check-equal? (count-char t #\f) 1)
  (check-equal? (count-char t #\o) 2)
  (check-equal? (count-char t #\o 0 1) 1)
  (check-equal? (count-char t #\newline) 2))


(define (str->text str)
  (define t (new text%))
  (send t insert-port (open-input-string str))
  t)

(define (map-indenter indenter t)
  (for/list ([line-idx (in-range (add1 (send t last-line)))])
            (indenter t (line-start t line-idx))))

(define (test-indenter indenter t-or-str)
  (define t (if (string? t-or-str) (str->text t-or-str) t-or-str))
  (list->string
   (append*
    (for/list ([line-idx (in-range (add1 (send t last-line)))]
               [indent (in-list (map-indenter indenter t))])
              ;; simulate DrR indentation
              ;; by dropping leading spaces and applying new indent.
              (append (make-list (or indent 0) #\space)
                      (dropf (line-chars t line-idx) (λ(x) (x . char=? . #\space))))))))
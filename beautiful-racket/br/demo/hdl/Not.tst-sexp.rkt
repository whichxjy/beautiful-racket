#lang s-exp br/demo/hdl-tst/expander


#|
load Not.hdl,
output-file Not.out,
compare-to Not.cmp,
output-list in%B3.1.3 out%B3.1.3;
set in 0,
eval,
output;
set in 1,
eval,
output;

|#

(require br/demo/hdl-tst/hdlprint rackunit racket/file)
(require "Not.hdl.rkt") ; load Not.hdl,
(define of (open-output-file "Not.out" #:mode 'text #:exists 'replace)) ; output-file Not.out,
(define (output in out) ; output-list in%B3.1.3 out%B3.1.3;
(fprintf of (format "~a\n" (string-join (list (hdlprint in "%B3.1.3") (hdlprint out "%B3.1.3")) "|" #:before-first "|" #:after-last "|"))))  
(define eval-result #f)
(define eval-thunk (λ () (list (Not-in) (Not-out)))) ; output-list in%B3.1.3 out%B3.1.3;
(output "in" "out") ; put names at top of output
(Not-in-write 0) ; set in 0,
(set! eval-result (eval-thunk)) ; eval,
(apply output eval-result) ; output;
(Not-in-write 1) ; set in 1,
(set! eval-result (eval-thunk)) ; eval,
(apply output eval-result) ; output;
(close-output-port of)
(display (file->string "Not.out"))
(check-equal? (file->lines "Not.out") (file->lines "Not.cmp")) ; compare-to Not.cmp,
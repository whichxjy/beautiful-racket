#lang br
(require br/test rackunit)

(check-equal? (run-source "atsign.rkt") "@")
(check-equal? (run-source "atsign-sexp.rkt") "@")
(check-equal? (run-source "hello.rkt") "Hello, World!")
(check-equal? (run-source "cow.rkt")
              "^__^\n(oo)\\_______\n(__)\\       )\\/\\\n    ||----w |\n    ||     ||\n")

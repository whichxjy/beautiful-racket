#lang at-exp br
(require br/test rackunit)
 
(check-equal? (run-source "jsonic-test.rkt") @string-append{
[
  null,
  42,
  true,
  ["array","of","strings"],
  {"key-2":false,"key-1":null,"key-3":{"subkey":21}}
]})
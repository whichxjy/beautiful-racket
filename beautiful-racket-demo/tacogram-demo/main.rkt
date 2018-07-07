#lang br/quicklang
(require "grammar.rkt")

(module+ reader
  (provide read-syntax))

(define (tokenize ip)
  (for/list ([tok (in-port read-char ip)])
            tok))

(define (leaf->char taco-leaf)
  (integer->char
   (for/sum ([val (in-list (cdr taco-leaf))]
             [power (in-naturals)]
             #:when (equal? val '(taco)))
            (expt 2 power))))

(define (read-syntax src ip)
  (define parse-tree (parse-to-datum (tokenize ip)))
  (define taco-branches (cdr parse-tree))
  (strip-context
   (with-syntax ([CHARS (map leaf->char taco-branches)])
     #'(module untaco racket
         (display (list->string 'CHARS))))))
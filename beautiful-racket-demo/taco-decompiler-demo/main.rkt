#lang br/quicklang

(module+ reader
  (provide read-syntax))

(define (tokenize ip)
  (for/list ([tok (in-port read ip)])
            tok))

(define (parse tok)
  (integer->char
   (for/sum ([val (in-list tok)]
             [power (in-naturals)]
             #:when (eq? val 'taco))
            (expt 2 power))))

(define (read-syntax src ip)
  (define toks (tokenize ip))
  (define parse-tree (map parse toks))
  (strip-context
   (with-syntax ([PT parse-tree])
     #'(module untaco racket
         (display (list->string 'PT))))))
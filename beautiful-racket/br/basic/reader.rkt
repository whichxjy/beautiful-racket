#lang br
(require br/reader-utils br/basic/parser br/basic/tokenizer)

(define-read-and-read-syntax (source-path input-port)
  (strip-context
   #`(module bf-mod br/basic/expander
       #,(parse source-path (tokenize input-port)))))

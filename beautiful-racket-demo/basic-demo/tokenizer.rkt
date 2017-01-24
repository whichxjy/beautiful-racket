#lang br
(require parser-tools/lex (prefix-in : parser-tools/lex-sre)
         brag/support racket/string)
(provide tokenize)

(define-lex-abbrevs
  (positive-integer (:+ numeric))
  ;; don't lex the leading "-": muddles "-X" and "Y-X"
  (positive-number (:or positive-integer (:seq (:? positive-integer) (:seq "." positive-integer)))))

(define (tokenize ip)
  (port-count-lines! ip)
  (define (next-token)
    (define get-token
      (lexer-src-pos
       [(eof) eof]
       [whitespace (token 'WHITE lexeme #:skip? #t)]
       [(from/to "/*" "*/") (token 'COMMENT lexeme #:skip? #t)]
       [(:: positive-number (:+ whitespace) (from/to (uc+lc "rem") "\n")) (token 'COMMENT lexeme #:skip? #t)]
       [(:or (uc+lc "print" "for" "to" "step" "if"
                   "goto" "input" "let" "next"
                   "return" "clear" "list" "run"
                   "end" "then" "else" "gosub"
                   "and" "or" "stop" "let" "def" "dim" "on")
            ";" "=" "(" ")" "+" "-" "*" "/" "^"
            "<=" ">=" "<>" "<" ">" "=" ":" ",") (string-downcase lexeme)]
       [positive-number (token 'NUMBER (string->number lexeme)
                               #:position (pos lexeme-start)
                               #:line (line lexeme-start)
                               #:column (col lexeme-start)
                               #:span (- (pos lexeme-end)
                                         (pos lexeme-start)))]
       [(:: alphabetic (:* (:or alphabetic numeric)) (:? "$")) (token 'ID (string->symbol lexeme)
                                                                  #:position (pos lexeme-start)
                                                                  #:line (line lexeme-start)
                                                                  #:column (col lexeme-start)
                                                                  #:span (- (pos lexeme-end)
                                                                            (pos lexeme-start)))]
       [(from/to "\"" "\"") (token 'STRING (trim-ends  "\"" lexeme  "\"")
                                   #:position (+ (pos lexeme-start) 1)
                                   #:line (line lexeme-start)
                                   #:column (+ (col lexeme-start) 1)
                                   #:span (- (pos lexeme-end)
                                             (pos lexeme-start) 2))]))
    (get-token ip))  
  next-token)
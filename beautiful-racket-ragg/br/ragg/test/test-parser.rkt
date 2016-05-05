#lang br


(require rackunit
         parser-tools/lex
         br/ragg/rules/parser
         br/ragg/rules/lexer
         br/ragg/rules/rule-structs)


;; quick-and-dirty helper for pos construction.
(define (p x)
  (pos x #f #f))



;; FIXME: fix the test cases so they work on locations rather than just offsets.
(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'")))
              (list (rule (p 1) (p 15)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-lit (p 8) (p 15) "hello" #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON")))
              (list (rule (p 1) (p 13)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-token (p 8) (p 13) "COLON" #f))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : <COLON> COLON")))
              (list (rule (p 1) (p 21)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 21)
                                   (list
                                    (pattern-token (p 8) (p 15) "COLON" #t)
                                    (pattern-token (p 16) (p 21) "COLON" #f))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'*")))
              (list (rule (p 1) (p 16)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-repeat (p 8) (p 16)
                                      0
                                      (pattern-lit (p 8) (p 15) "hello" #f)))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : 'hello'+")))
              (list (rule (p 1) (p 16)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-repeat (p 8) (p 16)
                                      1
                                      (pattern-lit (p 8) (p 15) "hello" #f)))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : [<'hello'>]")))
              (list (rule (p 1) (p 19)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-maybe (p 8) (p 19)
                                     (pattern-lit (p 9) (p 18) "hello" #t)))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON | BLAH")))
              (list (rule (p 1) (p 20)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-choice (p 8) (p 20)
                                      (list (pattern-token (p 8) (p 13) "COLON" #f)
                                            (pattern-token (p 16) (p 20) "BLAH" #f))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : COLON | BLAH | BAZ expr")))
              (list (rule (p 1) (p 31)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-choice (p 8) (p 31)
                                      (list (pattern-token (p 8) (p 13) "COLON" #f)
                                            (pattern-token (p 16) (p 20) "BLAH" #f)
                                            (pattern-seq (p 23) (p 31)
                                                     (list (pattern-token (p 23) (p 26) "BAZ" #f)
                                                           (pattern-id (p 27) (p 31) "expr" #f))))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : one two <three>")))
              (list (rule (p 1) (p 23)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 23) (list (pattern-id (p 8) (p 11) "one" #f)
                                              (pattern-id (p 12) (p 15) "two" #f)
                                              (pattern-id (p 16) (p 23) "three" #t))))))


(check-equal? (grammar-parser (tokenize (open-input-string "expr : (one two three)")))
              (list (rule (p 1) (p 23)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 23) (list (pattern-id (p 9) (p 12) "one" #f)
                                                          (pattern-id (p 13) (p 16) "two" #f)
                                                          (pattern-id (p 17) (p 22) "three" #f))))))


(check-equal? (grammar-parser (tokenize (open-input-string "expr : one two* three")))
              (list (rule (p 1) (p 22)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 22) (list (pattern-id (p 8) (p 11) "one" #f)
                                              (pattern-repeat (p 12) (p 16) 0 (pattern-id (p 12) (p 15) "two" #f))
                                              (pattern-id (p 17) (p 22) "three" #f))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : one two+ three")))
              (list (rule (p 1) (p 22)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 22) (list (pattern-id (p 8) (p 11) "one" #f)
                                              (pattern-repeat (p 12) (p 16) 1 (pattern-id (p 12) (p 15) "two" #f))
                                              (pattern-id (p 17) (p 22) "three" #f))))))

(check-equal? (grammar-parser (tokenize (open-input-string "expr : (one two)+ three")))
              (list (rule (p 1) (p 24)
                          (lhs-id (p 1) (p 5) "expr" #f)
                          (pattern-seq (p 8) (p 24) (list (pattern-repeat (p 8) (p 18) 1
                                                                          (pattern-seq (p 8) (p 17)
                                                                                       (list (pattern-id (p 9) (p 12) "one" #f)
                                                                                             (pattern-id (p 13) (p 16) "two" #f))))
                                                          (pattern-id (p 19) (p 24) "three" #f))))))


(check-equal? (grammar-parser (tokenize (open-input-string #<<EOF
statlist : stat+
stat: ID '=' expr
    | 'print' expr
EOF
)))
              (list (rule (p 1) (p 17)
                          (lhs-id (p 1) (p 9) "statlist" #f)
                          (pattern-repeat (p 12) (p 17) 1 (pattern-id (p 12) (p 16) "stat" #f)))
                    (rule (p 18) (p 54)
                          (lhs-id (p 18) (p 22) "stat" #f)
                          (pattern-choice (p 24) (p 54) (list (pattern-seq (p 24) (p 35) (list (pattern-token (p 24) (p 26) "ID" #f)
                                                                       (pattern-lit (p 27) (p 30) "=" #f)
                                                                       (pattern-id (p 31) (p 35) "expr" #f)))
                                                  (pattern-seq (p 42) (p 54) (list (pattern-lit (p 42) (p 49) "print" #f)
                                                                       (pattern-id (p 50) (p 54) "expr" #f))))))))


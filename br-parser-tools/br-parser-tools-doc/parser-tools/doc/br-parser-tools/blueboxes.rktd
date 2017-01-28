3346
((3) 0 () 5 ((q lib "br-parser-tools/lex.rkt") (q 628 . 6) (q lib "br-parser-tools/lex-sre.rkt") (q 836 . 6) (q lib "br-parser-tools/lex-plt-v200.rkt")) () (h ! (equal) ((c form c (c (? . 0) q return-without-pos)) q (602 . 2)) ((c def c (c (? . 0) q token?)) q (2067 . 3)) ((c form c (c (? . 0) q nothing)) q (1168 . 2)) ((c def c (c (? . 0) q position-line)) c (? . 1)) ((c form c (c (? . 0) q define-lex-abbrevs)) q (1405 . 2)) ((c form c (c (? . 0) q char-set)) q (1108 . 2)) ((c form c (c (? . 0) q define-empty-tokens)) q (1866 . 2)) ((c form c (c (? . 2) q seq)) q (1665 . 2)) ((c form c (c (? . 0) q start-pos)) q (538 . 2)) ((c def c (c (? . 0) q make-position)) c (? . 1)) ((c def c (c (? . 0) q position-token-token)) c (? . 3)) ((c form c (c (? . 0) q numeric)) q (1255 . 2)) ((c form c (c (? . 0) q lexer)) q (0 . 19)) ((c form c (c (? . 0) q any-string)) q (1150 . 2)) ((c form c (c (? . 0) q lexeme)) q (570 . 2)) ((c form c (c (? . 0) q upper-case)) q (1219 . 2)) ((c form c (c (? . 2) q =)) q (1550 . 2)) ((c def c (c (? . 0) q token-name)) q (1922 . 3)) ((q def ((lib "br-parser-tools/yacc-to-scheme.rkt") trans)) q (3193 . 3)) ((c form c (c (? . 2) q /)) q (1747 . 2)) ((c form c (c (? . 0) q alphabetic)) q (1183 . 2)) ((c form c (c (? . 4) q ~)) q (1796 . 2)) ((c def c (c (? . 0) q struct:position)) c (? . 1)) ((c form c (c (? . 0) q end-pos)) q (555 . 2)) ((c form c (c (? . 0) q symbolic)) q (1270 . 2)) ((c form c (c (? . 0) q whitespace)) q (1320 . 2)) ((c form c (c (? . 0) q define-lex-trans)) q (1448 . 2)) ((c def c (c (? . 0) q position-offset)) c (? . 1)) ((c form c (c (? . 2) q *)) q (1490 . 2)) ((c def c (c (? . 0) q struct:position-token)) c (? . 3)) ((c def c (c (? . 0) q token-value)) q (1995 . 3)) ((c form c (c (? . 0) q iso-control)) q (1351 . 2)) ((c form c (c (? . 0) q input-port)) q (584 . 2)) ((c form c (c (? . 2) q :)) q (1645 . 2)) ((c form c (c (? . 0) q lower-case)) q (1201 . 2)) ((c def c (c (? . 0) q position-token?)) c (? . 3)) ((q form ((lib "br-parser-tools/yacc.rkt") parser)) q (2119 . 23)) ((c form c (c (? . 2) q -)) q (1707 . 2)) ((c form c (c (? . 0) q punctuation)) q (1286 . 2)) ((c form c (c (? . 0) q blank)) q (1338 . 2)) ((c form c (c (? . 0) q lexer-src-pos)) q (486 . 2)) ((c form c (c (? . 2) q ~)) q (1727 . 2)) ((c def c (c (? . 0) q make-position-token)) c (? . 3)) ((c def c (c (? . 0) q position-token-start-pos)) c (? . 3)) ((c def c (c (? . 0) q position)) c (? . 1)) ((c def c (c (? . 0) q position-token)) c (? . 3)) ((q form ((lib "br-parser-tools/cfg-parser.rkt") cfg-parser)) q (2827 . 12)) ((c form c (c (? . 2) q or)) q (1624 . 2)) ((c form c (c (? . 2) q &)) q (1687 . 2)) ((c def c (c (? . 0) q file-path)) q (1023 . 4)) ((c form c (c (? . 2) q +)) q (1510 . 2)) ((c form c (c (? . 0) q define-tokens)) q (1816 . 2)) ((c form c (c (? . 2) q ?)) q (1530 . 2)) ((c form c (c (? . 0) q title-case)) q (1237 . 2)) ((c form c (c (? . 2) q **)) q (1597 . 2)) ((c form c (c (? . 4) q epsilon)) q (1779 . 2)) ((c def c (c (? . 0) q position-token-end-pos)) c (? . 3)) ((c form c (c (? . 0) q define-lex-abbrev)) q (1370 . 2)) ((c form c (c (? . 0) q any-char)) q (1134 . 2)) ((c def c (c (? . 0) q position?)) c (? . 1)) ((c form c (c (? . 0) q graphic)) q (1305 . 2)) ((c form c (c (? . 2) q >=)) q (1573 . 2)) ((c def c (c (? . 0) q position-col)) c (? . 1))))
syntax
(lexer [trigger action-expr] ...)
 
trigger = re
        | (eof)
        | (special)
        | (special-comment)
           
     re = id
        | string
        | character
        | (repetition lo hi re)
        | (union re ...)
        | (intersection re ...)
        | (complement re)
        | (concatenation re ...)
        | (char-range char char)
        | (char-complement re)
        | (id datum ...)
syntax
(lexer-src-pos (trigger action-expr) ...)
syntax
start-pos
syntax
end-pos
syntax
lexeme
syntax
input-port
syntax
return-without-pos
struct
(struct position (offset line col)
    #:extra-constructor-name make-position)
  offset : exact-positive-integer?
  line : exact-positive-integer?
  col : exact-nonnegative-integer?
struct
(struct position-token (token start-pos end-pos)
    #:extra-constructor-name make-position-token)
  token : any/c
  start-pos : position?
  end-pos : position?
parameter
(file-path) -> any/c
(file-path source) -> void?
  source : any/c
syntax
(char-set string)
syntax
any-char
syntax
any-string
syntax
nothing
syntax
alphabetic
syntax
lower-case
syntax
upper-case
syntax
title-case
syntax
numeric
syntax
symbolic
syntax
punctuation
syntax
graphic
syntax
whitespace
syntax
blank
syntax
iso-control
syntax
(define-lex-abbrev id re)
syntax
(define-lex-abbrevs (id re) ...)
syntax
(define-lex-trans id trans-expr)
syntax
(* re ...)
syntax
(+ re ...)
syntax
(? re ...)
syntax
(= n re ...)
syntax
(>= n re ...)
syntax
(** n m re ...)
syntax
(or re ...)
syntax
(: re ...)
syntax
(seq re ...)
syntax
(& re ...)
syntax
(- re ...)
syntax
(~ re ...)
syntax
(/ char-or-string ...)
syntax
(epsilon)
syntax
(~ re ...)
syntax
(define-tokens group-id (token-id ...))
syntax
(define-empty-tokens group-id (token-id ...))
procedure
(token-name t) -> symbol?
  t : (or/c token? symbol?)
procedure
(token-value t) -> any/c
  t : (or/c token? symbol?)
procedure
(token? v) -> boolean?
  v : any/c
syntax
(parser clause ...)
 
    clause = (grammar (non-terminal-id
                       ((grammar-id ...) maybe-prec expr)
                       ...)
                      ...)
           | (tokens group-id ...)
           | (start non-terminal-id ...)
           | (end token-id ...)
           | (error expr)
           | (precs (assoc token-id ...) ...)
           | (src-pos)
           | (suppress)
           | (debug filename)
           | (yacc-output filename)
              
maybe-prec = 
           | (prec token-id)
              
     assoc = left
           | right
           | nonassoc
syntax
(cfg-parser clause ...)
 
clause = (grammar (non-terminal-id
                   ((grammar-id ...) maybe-prec expr)
                   ...)
                  ...)
       | (tokens group-id ...)
       | (start non-terminal-id ...)
       | (end token-id ...)
       | (error expr)
       | (src-pos)
procedure
(trans file) -> any/c
  file : path-string?

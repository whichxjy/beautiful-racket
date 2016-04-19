#lang ragg
;; adapted from http://www.ittybittycomputers.com/IttyBitty/TinyBasic/TBuserMan.txt

;; MS Basic extensions
;; http://www.atariarchives.org/basicgames/showpage.php?page=i12

;; games
;; http://www.vintage-basic.net/games.html

;; chipmunk basic
;; http://www.nicholson.com/rhn/basic/basic.man.html

basic-program : [CR] lines [CR]

lines : INTEGER statements [CR | CR lines]

statements : statement [":" statements]

statement : "CLOSE" "#" INTEGER
| "END"
| "FOR" ID "=" expr "TO" expr ["STEP" expr]     
| "GOTO" expr
| "IF" expr "THEN" (statement | expr) ; change: add expr
| "INPUT" id-list
| ["LET"] ID "=" expr ; change: make "LET" opt
| "NEXT" id-list
| "PRINT" printlist
| "REM" STRING

id-list : ID ["," id-list]

value-list : value ["," value-list]

constant-list : constant ["," constant-list]

integer-list : INTEGER ["," integer-list]

expr-list : expr ["," expr-list]

printlist : [expr [";" printlist]]

expr : and-expr ["OR" expr]

and-expr : not-expr ["AND" and-expr]

not-expr : ["NOT"] compare-expr

compare-expr : add-expr [("=" | "<>" | "><" | ">" | ">=" | "<" | "<=") compare-expr]

add-expr : mult-expr [("+" | "-") add-expr]

mult-expr : negate-expr [("*" | "/") mult-expr]

negate-expr : ["-"] power-expr

power-expr : [power-expr "^"] value

value : "(" expr ")"
| ID ["(" expr-list ")"]
| constant

constant : INTEGER | STRING | REAL

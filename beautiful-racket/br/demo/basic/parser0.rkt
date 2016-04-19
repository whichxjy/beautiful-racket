#lang ragg
;; adapted from http://www.ittybittycomputers.com/IttyBitty/TinyBasic/TBuserMan.txt

;; MS Basic extensions
;; http://www.atariarchives.org/basicgames/showpage.php?page=i12

;; games
;; http://www.vintage-basic.net/games.html

;; chipmunk basic
;; http://www.nicholson.com/rhn/basic/basic.man.html

basic-program : [CR] line [CR line]* [CR]

line: INTEGER statements

statements : statement [":" statement]*

statement : "END"
| "FOR" ID "=" expr "TO" expr ["STEP" expr]     
| "GOTO" expr
| "IF" expr "THEN" (statement | expr) ; change: add expr
| "INPUT" id-list
| ["LET"] ID "=" expr ; change: make "LET" opt
| "NEXT" id-list
| "PRINT" print-list
| "REM" STRING

id-list : ID ["," id-list]

;value-list : value ["," value]*

;datum-list : datum ["," datum]*

;integer-list : INTEGER ["," INTEGER]*

expr-list : expr ["," expr]*

print-list : [expr [";" print-list]]

;expr : and-expr ["OR" expr]
;and-expr : not-expr ["AND" and-expr]
;not-expr : ["NOT"] compare-expr
;compare-expr : term [("=" | "<>" | "><" | ">" | ">=" | "<" | "<=") compare-expr]

expr : term [("=" | "<>" | "><" | ">" | ">=" | "<" | "<=") expr]


term : factor [("+" | "-") term]

factor : value [("*" | "/") factor]

;negate-expr : ["-"] power-expr

;power-expr : [power-expr "^"] value

value : "(" expr ")"
| ID ["(" expr-list ")"]
| datum

datum : INTEGER | STRING | REAL

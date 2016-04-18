#lang ragg
;; adapted from http://www.ittybittycomputers.com/IttyBitty/TinyBasic/TBuserMan.txt

;; MS Basic extensions
;; http://www.atariarchives.org/basicgames/showpage.php?page=i12

;; games
;; http://www.vintage-basic.net/games.html


basic-program : [CR] line (CR line)* [CR]

line: [NUMBER] statement (":" statement)*

statement : "PRINT" printlist*
| "PR" printlist
| "INPUT" varlist
| "LET" var "=" expression
| var "=" expression
| "GOTO" expression
| "GOSUB" expression
| "RETURN"
| "IF" expression relop expression "THEN" statement
| "IF" expression relop expression statement
| "CLEAR"
| "RUN"
| "RUN" exprlist
| "LIST"
| "LIST" exprlist

; formerly printlist : printitem [(":" | (separator printitem)*)]
printlist :  printitem (separator printitem)*

printitem : expression | STRING

varlist: var ("," var)*

exprlist : expression ("," expression)*

expression : [("+"|"-")] unsignedexpr

unsignedexpr : term [("+"|"-") unsignedexpr]

term : factor [("*"|"/") term]

factor : var
| number
| "(" expression ")"
| function

function : "RND(" expression ")"
| "USR(" exprlist ")"
| "TAB(" expression ")"

number : NUMBER

separator : "," | ";"

var : "A" | "B" | "C" | "D" | "T"

digit: DIGIT

relop : "<" [("="|">")] | ">" [("="|"<")] | "="
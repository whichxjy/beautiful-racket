#lang ragg
;; adapted from http://www.ittybittycomputers.com/IttyBitty/TinyBasic/TBuserMan.txt

basic-program : line*

line : NUMBER statement CR | statement CR | CR

statement : "PRINT" printlist
| "PR" printlist
| "INPUT" varlist
| "LET" var "=" expression
| var "=" expression
| "GOTO" expression
| "GOSUB" expression
| "RETURN"
| "IF" expression relop expression "THEN" statement
| "IF" expression relop expression statement
;| "REM" commentstring ; todo: implement in tokenizer
| "CLEAR"
| "RUN"
| "RUN" exprlist
| "LIST"
| "LIST" exprlist

printlist : printitem [(":" | separator printlist)]

printitem : expression | STRING

varlist: var ["," varlist]

exprlist : expression ["," exprlist]

expression : [("+"|"-")] unsignedexpr

unsignedexpr : term [("+"|"-") unsignedexpr]

term : factor [("*"|"/") term]

factor : var
| number
| "(" expression ")"
| function

function : "RND(" expression ")"
| "USR(" exprlist ")"

number : NUMBER

separator : "," | ";"

var : UPPERCASE

digit: DIGIT

relop : "<" [("="|">")] | ">" [("="|"<")] | "="
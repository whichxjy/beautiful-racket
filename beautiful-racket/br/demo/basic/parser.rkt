#lang brag

basic-program : line*

line: NUMBER statement [/":" statement]*

statement : "end"
| "gosub" expr
| "goto" expr
| "if" expr /"then" (statement | expr) [/"else" (statement | expr)]
| "input" [print-list /";"] ID
| ID "=" expr
| "print" [print-list]
| "return"

print-list : expr [";" [print-list]]

expr : comp-expr [("AND" | "OR") expr]

comp-expr : sum [("=" | ">" | ">=" | "<" | "<=" | "<>") comp-expr]

sum : [sum ("+" | "-")] product

product : [product ("*" | "/")] value 

@value : ID
| id-expr
| /"(" expr /")"
| STRING
| NUMBER

/id-expr : ID [/"(" expr [/"," expr]* /")"]
#lang brag

program : line*

line: NUMBER statement [":" statement]*

statement : "END"
| "GOSUB" NUMBER
| "GOTO" expr
| "IF" expr "THEN" (statement | expr) ["ELSE" (statement | expr)]
| "INPUT" [print-list ";"] ID
| ID "=" expr ; change: make "LET" opt
| "PRINT" print-list
| "RETURN"

print-list : [expr [";" [print-list]]]

expr : comp-expr [("AND" | "OR") expr]

comp-expr : sum [("=" | ">" | ">=" | "<" | "<=" | "<>") comp-expr]

sum : product [("+" | "-") sum]

product : value [("*" | "/") product]

@value : ID | id-expr
| /"(" expr /")"
| STRING
| NUMBER

/id-expr : ID [/"(" expr [/"," expr]* /")"]
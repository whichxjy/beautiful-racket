#lang ragg

;; recursive rules destucture easily in the expander
program : [CR]* [line [CR line]*] [CR]*

line: NUMBER statement-list

statement-list : statement [":" statement-list]

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

value : "(" expr ")"
| ID ["(" expr* ")"]
| STRING
| NUMBER


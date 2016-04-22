#lang ragg

;; recursive rules destucture easily in the expander
program : [line [CR line]*]

line: INTEGER statement+

statement : "END"
| "FOR" ID "=" expr "TO" expr ["STEP" expr]
| "GOSUB" INTEGER
| "GOTO" expr
| "IF" expr "THEN" (statement | expr) ["ELSE" (statement | expr)]; change: add expr
| "INPUT" [print-list ";"] ID
| ["LET"] ID "=" expr ; change: make "LET" opt
| "NEXT" ID+
| "PRINT" print-list
| "RETURN"
| REM-COMMENT

print-list : [expr [";" [print-list]]]

expr : comp-expr [("AND" | "OR") expr]

comp-expr : sum [("=" | ">" | ">=" | "<" | "<=" | "<>") comp-expr]

sum : product [("+" | "-") sum]

product : value [("*" | "/") product]

value : "(" expr ")"
| ID
| PROC "(" expr* ")"
| INTEGER
| STRING
| REAL


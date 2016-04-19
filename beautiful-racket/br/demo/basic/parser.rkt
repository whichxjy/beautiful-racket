#lang ragg

basic-program : cr-line* [CR]

cr-line : CR line [cr-line]

line: INTEGER statement+

statement : "END"
| "FOR" ID "=" expr "TO" expr ["STEP" expr]     
| "GOTO" expr
| "IF" expr "THEN" (statement | expr) ; change: add expr
| "INPUT" ID+
| ["LET"] ID "=" expr ; change: make "LET" opt
| "NEXT" ID+
| "PRINT" print-list
| REM-COMMENT

print-list : [expr [";" [print-list]*]]

expr : sum [("=" | "<>" | "><" | ">" | ">=" | "<" | "<=") expr]

sum : product [("+" | "-") sum]+

product : value [("*" | "/") product]+

value : "(" expr ")"
| ID ["(" expr* ")"]
| INTEGER
| STRING
| REAL


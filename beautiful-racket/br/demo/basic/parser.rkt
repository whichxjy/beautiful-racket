#lang ragg

;; recursive rules destucture easily in the expander
program : [line [CR line]*]

line: INTEGER statement+

statement : "END"
| "FOR" ID "=" expr "TO" expr ["STEP" expr]     
| "GOTO" expr
| "IF" expr "THEN" (statement | expr) ; change: add expr
| "INPUT" print-list ";" ID
| ["LET"] ID "=" expr ; change: make "LET" opt
| "NEXT" ID+
| "PRINT" print-list
| REM-COMMENT

print-list : [expr [";" [print-list]]]

expr : sum [("=" | ">" | ">=" | "<" | "<=" | "<>") expr]

sum : product [("+" | "-") sum]

product : value [("*" | "/") product]

value : ID ["(" expr* ")"]
| INTEGER
| STRING
| REAL


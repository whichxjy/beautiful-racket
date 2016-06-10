#lang brag

basic-program : line*

line: NUMBER statement [/":" statement]*

statement : "def" id /"(" id /")" /"=" expr
| "dim" id-expr [/"," id-expr]*
| "end" | "stop"
| "gosub" expr
| "goto" expr
| "on" expr ("gosub" | "goto") expr [/"," expr]*
| "if" expr /"then" (statement | expr) [/"else" (statement | expr)]
| "input" [print-list /";"] id [/"," id]*
| [/"let"] id-expr "=" expr
| "print" [print-list]
| "return"
| "for" id /"=" value /"to" value [/"step" value]
| "next" [id]

print-list : expr [[";"] [print-list]]

expr : comp-expr [("and" | "or") expr]

comp-expr : sum [("=" | ">" | ">=" | "<" | "<=" | "<>") comp-expr]

sum : [sum ("+" | "-")] product

product : [product ("*" | "/")] value 

@value : id-val
| id-expr
| /"(" expr /")"
| number
| STRING

id-expr : id [/"(" expr [/"," expr]* /")"]

@id : ID

id-val : ["-"] id-expr

number : ["-"] NUMBER
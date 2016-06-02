#lang brag

basic-program : line*

line: NUMBER statement [/":" statement]*

statement : "end" | "stop"
| "gosub" expr
| "goto" expr
| "if" expr /"then" (statement | expr) [/"else" (statement | expr)]
| "input" [print-list /";"] id [/"," id]*
| [/"let"] id "=" expr
| "print" [print-list]
| "return"
| "for" id /"=" value /"to" value [/"step" value]
| "next" [id]

print-list : expr [[";"] [print-list]]

expr : comp-expr [("and" | "or") expr]

comp-expr : sum [("=" | ">" | ">=" | "<" | "<=" | "<>") comp-expr]

sum : [sum ("+" | "-")] product

product : [product ("*" | "/")] value 

@value : id
| id-expr
| /"(" expr /")"
| NUMBER
| STRING

/id-expr : id [/"(" expr [/"," expr]* /")"]

@id : ID

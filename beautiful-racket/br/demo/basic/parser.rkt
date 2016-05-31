#lang brag

basic-program : line*

line: NUMBER statement [/":" statement]*

statement : "end" | "stop"
| "gosub" expr
| "goto" expr
| "if" expr /"then" (statement | expr) [/"else" (statement | expr)]
| "input" [print-list /";"] ID [/"," ID]*
| [/"let"] ID "=" expr
| "print" [print-list]
| "return"
| "for" ID /"=" value /"to" value [/"step" value]
| "next" [ID]

print-list : expr [[";"] [print-list]]

expr : comp-expr [("and" | "or") expr]

comp-expr : sum [("=" | ">" | ">=" | "<" | "<=" | "<>") comp-expr]

sum : [sum ("+" | "-")] product

product : [product ("*" | "/")] value 

@value : ID
| id-expr
| /"(" expr /")"
| NUMBER
| STRING

/id-expr : ID [/"(" expr [/"," expr]* /")"]
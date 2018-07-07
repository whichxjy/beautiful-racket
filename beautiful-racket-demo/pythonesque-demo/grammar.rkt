#lang brag

top : @statement*
statement : assignment | func-def | expr | return | for | if | print
assignment : ID /"=" expr
@expr : comparison
comparison : [comparison ("<" | ">")] sum
sum : [sum ("+" | "-")] product
product : [product ("*" | "/")] value
@value : ID | INTEGER | func-app | STRING
func-app : ID /"(" @exprs /")"
exprs : [expr (/"," expr)*]
func-def : /"def" ID /"(" ids /")" /":" @block
/ids : [ID (/"," ID)*]
block : /INDENT @statement* /DEDENT
return : /"return" expr
for : /"for" ID /"in" expr /":" @block
if : /"if" expr /":" block [/"else" /":" block]
print : /"print" expr
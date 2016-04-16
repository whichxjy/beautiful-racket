#lang ragg

basic-program : line*

line : CR | NUMBER statement CR | statement CR
| NUMBER statement | statement

statement : "PRINT" expr-list
| "IF" expression relop expression "THEN" statement
| "GOTO" expression
| "INPUT" var-list
| "LET" var "=" expression
| "GOSUB" expression
| "RETURN"
| "CLEAR"
| "LIST"
| "RUN"
| "END"

expr-list : (STRING | expression) ("," (STRING | expression) )*

var-list : var ("," var)*

expression : term (("+"|"-") term)*

term : factor (("*"|"/") factor)*

factor : var | NUMBER | (expression)

var : UPPERCASE

relop : "<" (">"|"="|"ε") | ">" ("<"|"="|"ε") | "="
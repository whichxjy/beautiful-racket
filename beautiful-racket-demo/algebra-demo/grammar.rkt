#lang brag

top : (func-def | func-app)*
func-def : /"fun" var /"(" vars /")" /"=" expr
/vars : var [/"," var]
expr : var "+" var | value
@value : var | INT | func-app
func-app : var /"(" value [/"," value] /")"
@var : ID

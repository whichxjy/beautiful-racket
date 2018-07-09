#lang brag

top : (func-def | func-app)*
func-def : /"fun" var /"(" boundvars /")" /"=" expr
/boundvars : var [/"," var]
expr : var "+" var | func-app
func-app : var /"(" arg [/"," arg] /")"
@var : ID
@arg : var | INT
#lang brag

top : func-def{2} func-app
func-def : /"fun" ID /"(" argids /")" /"=" expr
/argids : ID [/"," ID]
expr : ID "+" ID | func-app
func-app : ID /"(" arg [/"," arg] /")"
@arg : ID | INT
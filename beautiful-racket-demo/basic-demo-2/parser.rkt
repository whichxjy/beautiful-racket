#lang brag
;; program & lines
b-program : [b-line] (/NEWLINE [b-line])*
b-line : b-line-number [b-statement] (/":" [b-statement])* [b-rem]
@b-line-number : INTEGER
b-rem : REM

;; statements
@b-statement : b-end | b-print | b-let | b-input | b-def
             | b-goto | b-gosub | b-return | b-for | b-next | b-if
b-end : /"end"
b-print : /"print" [STRING | b-expr] (/";" [STRING | b-expr])*
b-goto : /"goto" b-expr
b-if : /"if" b-expr /"then" b-expr [/"else" b-expr]
b-gosub : /"gosub" b-expr
b-return : /"return"
b-input : /"input" b-id
b-def :  /"def" b-id /"(" b-id /")" /"=" b-expr
b-let : [/"let"] b-id /"=" [STRING | b-expr]
b-for : /"for" b-id /"=" b-expr /"to" b-expr [/"step" b-expr]
b-next : /"next" [b-id]

;; expressions with precedence & order
b-expr : b-logic-expr 
b-logic-expr : [b-logic-expr ("and" | "or")] b-comp-expr
b-comp-expr : [b-comp-expr ("=" | "<" | ">")] b-sum
b-sum : [b-sum ("+"|"-")] b-product
b-product : [b-product ("*"|"/"|"%"|"^")] b-value

;; values
@b-value : b-id | b-number | /"(" b-expr /")" | b-not | b-func
/b-func : b-id /"(" b-expr /")"
b-not : /"!" b-value
@b-id : ID
@b-number : b-positive | b-negative
@b-positive : INTEGER | DECIMAL
b-negative : /"-" b-positive
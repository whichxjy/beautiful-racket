#lang brag
b-program : [b-line] (/NEWLINE [b-line])*
b-line : b-line-number [b-statement] (/":" [b-statement])*
@b-line-number : INTEGER
@b-statement : b-rem | b-end | b-print | b-let
             | b-goto | b-gosub | b-return | b-for | b-next
b-rem : REM
b-end : /"end"
b-print : /"print" [STRING | b-num-expr]
b-goto : /"goto" b-num-expr
b-gosub : /"gosub" b-num-expr
b-return : /"return"
b-let : [/"let"] b-id /"=" b-num-expr
b-for : /"for" b-id /"=" b-num-expr /"to" b-num-expr [/"step" b-num-expr]
b-next : /"next" [b-id]
@b-id : ID
b-num-expr : b-sum
b-sum : (b-value /"+" b-value)*
@b-value : b-id | b-number
@b-number : b-positive | b-negative
@b-positive : INTEGER | DECIMAL
b-negative : /"-" b-positive
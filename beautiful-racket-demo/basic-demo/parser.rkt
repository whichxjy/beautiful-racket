#lang brag
b-program : [b-line] (/NEWLINE [b-line])*
b-line : @b-line-number [b-statement] (/":" [b-statement])*
b-line-number : INTEGER
@b-statement : b-rem | b-end | b-print | b-goto
b-rem : REM
b-end : /"end"
b-print : /"print" [STRING | b-num-expr]
b-goto : /"goto" b-num-expr
b-num-expr : b-sum
b-sum : (b-number /"+")* b-number
@b-number : INTEGER | DECIMAL
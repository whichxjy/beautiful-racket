#lang brag
b-program : b-line*
b-line : @b-line-number b-statement (/":" b-statement)*
b-line-number : INTEGER
@b-statement : b-rem
             | b-print
             | b-goto
             | b-end
b-rem : REM
b-print : /"print" (STRING | b-expr)*
b-goto : /"goto" b-expr
b-expr : b-sum
b-sum : (b-number /"+")* b-number
@b-number : INTEGER | DECIMAL
b-end : /"end"
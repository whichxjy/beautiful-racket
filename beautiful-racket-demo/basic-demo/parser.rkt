#lang brag

b-program : b-line*

b-line: @b-line-number b-statement

b-line-number : NUMBER

b-statement: b-rem
| b-print
| b-goto
| b-end

b-rem : REM

b-print : /"print" STRING

b-goto : /"goto" NUMBER

b-end : /"end"
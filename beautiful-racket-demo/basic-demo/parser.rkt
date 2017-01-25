#lang brag

b-program : b-line*

b-line: NUMBER b-statement

b-statement: b-rem
| b-print
| b-goto
| b-end

b-rem : REM

b-print : /"print" STRING

b-goto : /"goto" NUMBER

b-end : /"end"
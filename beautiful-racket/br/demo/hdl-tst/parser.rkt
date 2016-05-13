#lang brag

tst-program : header-expr test-expr*

header-expr : load-expr table-expr /";"

@load-expr : /"load" ID /","

/table-expr : /"output-list" columns

@columns :  ID [/"," columns]

test-expr : step-expr+ /";"

@step-expr : (set-expr | @eval-expr | output-expr) [/","]

/set-expr : /"set" ID VAL

eval-expr : /"eval"

output-expr : /"output"
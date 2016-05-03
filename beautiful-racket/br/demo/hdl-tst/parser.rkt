#lang br/ragg

tst-program : header-expr test-expr*

header-expr : load-expr table-expr ";"

load-expr : "load" filename ","

filename : ID

table-expr : "output-list" column-id+

column-id :  ID [","]

test-expr : step-expr+ ";"

step-expr : (set-expr | eval-expr | output-expr) [","]

set-expr : "set" ID VAL

eval-expr : "eval"

output-expr : "output"
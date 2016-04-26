#lang ragg

tst-program : load-expr header-expr test-expr*

load-expr : "load" ID ","

header-expr : "output-list" ID comma-id* "," "out" ";"

comma-id : "," ID

test-expr : step-expr comma-step* ";"

comma-step : "," step-expr

step-expr : set-expr | eval-expr | output-expr

set-expr : "set" ID VAL

eval-expr : "eval"

output-expr : "output"
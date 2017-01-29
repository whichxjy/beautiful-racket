#lang brag

tst-program : tst-load-expr tst-output-file-expr tst-compare-to-expr tst-output-list-expr /";" tst-test-expr*
tst-load-expr : /"load" ID /","
tst-output-file-expr : /"output-file" ID /","
tst-compare-to-expr : /"compare-to" ID /","
tst-output-list-expr : /"output-list" tst-column [tst-column]+
/tst-column :  ID FORMAT-STRING
@tst-test-expr : tst-step-expr+ /";"
@tst-step-expr : (tst-set-expr | tst-eval-expr | tst-output-expr) [/","]
tst-set-expr : /"set" ID VAL
tst-eval-expr : /"eval"
tst-output-expr : /"output"
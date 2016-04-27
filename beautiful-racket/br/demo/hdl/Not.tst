#lang br/demo/hdl/tst

/* Not */

load Not.hdl,
output-list in, out;
set in 0,
eval, output;
set in 1,
eval, output;
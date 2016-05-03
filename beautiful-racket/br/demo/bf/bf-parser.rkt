#lang br/ragg
bf-program : (op | loop)*
op : ">" | "<" | "+" | "-" | "." | ","
loop : "[" (op | loop)* "]"
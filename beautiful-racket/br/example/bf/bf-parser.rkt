#lang ragg
bf-program : (op | loop)*
op : ">" | "<" | "+" | "-" | "." | ","
loop : "[" (op | loop)* "]"
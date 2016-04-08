#lang ragg
bf-program : expr*
expr : ">"
       | "<" 
       | "+" 
       | "-" 
       | "." 
       | "," 
       | loop
loop : "[" expr* "]"
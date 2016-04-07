#lang ragg

<expr> : ">"
       | "<" 
       | "+" 
       | "-" 
       | "." 
       | "," 
       | <loop>
<loop> : "["<expr>*"]"
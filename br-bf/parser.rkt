#lang ragg
;; use uppercase TOKEN-IDENTIFIERS for classes of tokens
;; too numerous to indicate individually
;; (e.g., numbers, strings)
bf-program : op*
op : ">" | "<" | "+" | "-" | "." | "," | loop
loop : "[" op* "]"
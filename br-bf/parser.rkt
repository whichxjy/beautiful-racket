#lang ragg
;; use uppercase TOKEN-IDENTIFIERS for classes of tokens
;; too numerous to indicate individually
;; (e.g., numbers, strings)

bf-program : (op | loop)*
op : ">" | "<" | "+" | "-" | "." | ","
loop : "[" (op | loop)* "]"


;; Alternate ways of specifying grammar
;; bf-program : op*
;; op : ">" | "<" | "+" | "-" | "." | "," | loop
;; loop : "[" op* "]"

;; bf-program : expr*
;; expr : op | loop
;; op : ">" | "<" | "+" | "-" | "." | ","
;; loop : "[" bf-program "]"


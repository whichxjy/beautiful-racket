#lang ragg
;; use uppercase TOKEN-IDENTIFIERS for classes of tokens
;; too numerous to indicate individually
;; (e.g., numbers, strings)

;; parser imposes structure:
;; takes a flat list of tokens
;; and arranges them into an (often hierarchical / recursive) shape.
;; produces a parse tree, which is like an annotated, structured version of the source code.
;; gives us the parenthesized expressions we need for the expander.


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


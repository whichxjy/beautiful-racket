#lang br/ragg

;; rule of thumb: use [optional] bits judiciously as they multiply the cases needed for a production rule
;; rule of thumb: for a set of related IDs, put each into the same grammar entity
;; rule of thumb: avoid mushing unrelated IDs into one grammar entity
;; whereas a * corresponds directly to an ... in the expander macro
;; syntax patterns are good for
;; + single case / nonrecursive structure
;; + nonalternating pattern (no "this that this that ...")

chip-program : "CHIP" ID "{" pin-spec pin-spec part-spec "}"

pin-spec : ("IN" | "OUT") pin+ ";"

pin : ID [","]

part-spec : "PARTS:" part+

part : ID "(" pin-val-pair+ ")" ";"

pin-val-pair : ID "=" ID [","]
#lang ragg

chip-program : "CHIP" ID "{" pin-spec pin-spec part-spec "}"

pin-spec : ("IN" | "OUT") pin-list ";"

pin-list : ID comma-id*

comma-id : "," ID

part-spec : "PARTS:" part-list

part-list : part+

part : ID "(" ID "=" ID comma-id-pair* ")" ";"

comma-id-pair : "," ID "=" ID
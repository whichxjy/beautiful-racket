#lang ragg

chip-program : "CHIP" ID "{" pin-spec pin-spec part-spec "}"

pin-spec : ("IN" | "OUT") pin-list ";"

pin-list : ID another-id*

another-id : "," ID

part-spec : "PARTS:" part-list

part-list : part+

part : ID "(" ID "=" ID another-id-pair* ")" ";"

another-id-pair : "," ID "=" ID
#lang ragg

chip-program : "CHIP" ID "{" pin-spec "}"

pin-spec : ("IN" | "OUT") pin-list ";"

pin-list : ID ["," pin-list]

part-spec : "PARTS:" part+

part : ID "(" part-arg-list ")" ";"

part-arg-list : ID "=" ID ["," part-arg-list]
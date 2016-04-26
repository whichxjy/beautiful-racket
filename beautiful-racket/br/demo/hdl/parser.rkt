#lang ragg

chip-program : "CHIP" ID "{" pin-spec-in pin-spec-out part-spec "}"

pin-spec-in : "IN" pin-list ";"

pin-spec-out : "OUT" pin-list ";"

pin-list : ID ["," ID]*

part-spec : "PARTS:" part-list

part-list : [part ";"]+

part : ID "(" ID "=" ID ["," ID "=" ID]* ")"

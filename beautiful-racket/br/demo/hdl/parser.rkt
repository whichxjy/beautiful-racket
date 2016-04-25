#lang ragg

chip-program : "CHIP" ID "{" pin-spec-in pin-spec-out part-spec "}"

pin-spec-in : "IN" ID ["," ID]* ";"

pin-spec-out : "OUT" ID ["," ID]* ";"


part-spec : "PARTS:" [ID "(" ID "=" ID "," [ ID "=" ID ","]* "out" "=" ID ")" ";"]+

#lang ragg

chip-program : "CHIP" chip-name "{" in-pin-spec "}"

chip-name : ID

in-pin-spec : "IN" pin-list ";"

out-pin-spec : "OUT" pin-list ";"

pin-list : pin ["," pin-list]

pin : ID

part-spec : "PARTS:" part+

part : ID "(" part-arg-list ")" ";"

part-arg-list : part-arg ["," part-arg-list]

part-arg : ID "=" ID
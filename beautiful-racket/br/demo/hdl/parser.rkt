#lang brag

chip-program : /"CHIP" chipname /"{" pin-spec pin-spec part-spec /"}"

@chipname : ID

pin-spec : (/"IN" | /"OUT") pin [/"," pin]* /";"

@pin : ID

part-spec : /"PARTS:" part+

part : partname /"(" pin-val-pair [/"," pin-val-pair]* /")" /";"

@partname : ID

/pin-val-pair : pin /"=" ID
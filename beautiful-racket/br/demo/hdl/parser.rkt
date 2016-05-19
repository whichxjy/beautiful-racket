#lang brag

chip-program : /"CHIP" chipname /"{" in-spec out-spec part-spec /"}"

@chipname : ID

in-spec : pin-spec

out-spec : pin-spec

@pin-spec : (/"IN" | /"OUT") pin [/"," pin]* /";"

/pin : ID  [/"[" NUMBER /"]"]

@part-spec : /"PARTS:" part+

part : partname /"(" pin-val-pair [/"," pin-val-pair]* /")" /";"

@partname : ID

/pin-val-pair : ID  [/"[" bus-range /"]"] /"=" pin-val

@bus-range : (NUMBER | BINARY-NUMBER) [/"." /"." (NUMBER | BINARY-NUMBER)]

@pin-val : ID
         | BINARY-NUMBER
         | TRUE
         | FALSE

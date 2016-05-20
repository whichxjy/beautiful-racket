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

/pin-val-pair : pin-range /"=" pin-val

@bus-range : number [/"." /"." number]

@pin-range : ID [/"[" bus-range /"]"]

@pin-val : pin-range
         | BINARY-NUMBER
         | TRUE
         | FALSE

@number : BINARY-NUMBER | NUMBER
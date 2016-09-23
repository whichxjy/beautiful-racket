#lang brag

jsonic-program: value*

value: array | object | string | number

object: "{" [kvpair ("," kvpair)*] "}"

array: "[" [value ("," value)*] "]"

string: STRING

number: NUMBER

kvpair: STRING ":" value

s-val: "@" ("[" STRING "]" | "{" STRING "}" | "#" STRING "#" )

            
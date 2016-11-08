#lang brag
jsonic-program: (s-exp | json-char)*
s-exp: /OPEN CHAR* /CLOSE
json-char: CHAR

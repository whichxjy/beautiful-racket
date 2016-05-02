#lang ragg

txtadv-program : [verb-section] 

verb-section : verb-heading verb-entry+

verb-heading : "===VERBS==="

verb-entry : verb-name+ desc

verb-name : [","] ID ["_"]

desc : s-exp

s-exp : ID | STRING | "(" s-exp* ")"
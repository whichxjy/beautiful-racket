#lang ragg

txtadv-program : [verb-section] [everywhere-section] 

verb-section : "===VERBS===" verb-entry+

verb-entry : verb-name+ desc

verb-name : [","] ID ["_"]

everywhere-section : "===EVERYWHERE===" everywhere-action+

everywhere-action : ID desc

desc : s-exp

s-exp : ID | STRING | "(" s-exp* ")"
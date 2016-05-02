#lang ragg

txtadv-program : [verb-section] [everywhere-section] [things-section]

verb-section : "===VERBS===" verb-entry+

verb-entry : verb-name+ desc

verb-name : [","] ID ["_"]

everywhere-section : "===EVERYWHERE===" everywhere-action+

everywhere-action : ID desc

things-section : "===THINGS===" thing-entry+

thing-entry : thing-id thing-action+

thing-id : THING-NAME

thing-action : ID desc

desc : s-exp

s-exp : ID | STRING | ("(" "[" "{") s-exp* (")" "]" "}")
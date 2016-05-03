#lang ragg

txtadv-program : [verb-section] [everywhere-section] [things-section]

verb-section : "===VERBS===" verb-item+

verb-item : verb-name+ desc

verb-name : [","] ID ["_"]

everywhere-section : "===EVERYWHERE===" everywhere-item+

everywhere-item : ID desc

things-section : "===THINGS===" thing-item+

thing-item : thing-id thing-action+

thing-id : THING-NAME

thing-action : ID desc

desc : s-exp

s-exp : ID | STRING | ("(" | "[" | "{") s-exp* (")" | "]" | "}")
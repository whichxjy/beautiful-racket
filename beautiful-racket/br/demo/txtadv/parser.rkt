#lang ragg

txtadv-program : [verb-section] [everywhere-section] [things-section] places-section start-section

verb-section : "===VERBS===" verb-item+

verb-item : verb-name+ desc

verb-name : [","] ID ["_"]

everywhere-section : "===EVERYWHERE===" everywhere-item+

everywhere-item : ID desc

things-section : "===THINGS===" thing-item+

thing-item : thing-id thing-action+

thing-id : DASHED-NAME

thing-action : ID desc

places-section : "===PLACES===" place-item+

place-item : place-id place-descrip place-items place-action+

place-id : DASHED-NAME

place-descrip : STRING ; place-desc is already used in expander

place-items : "[" place-name* "]" ; place-things is already used

place-name : [","] ID

place-action : ID desc

start-section : "===START===" place-name

desc : s-exp

s-exp : ID | STRING | ("(" | "[" | "{") s-exp* (")" | "]" | "}")
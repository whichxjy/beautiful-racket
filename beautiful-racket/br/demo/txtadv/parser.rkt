#lang brag

txtadv-program : verb-section everywhere-section things-section places-section start-section

verb-section : !"===VERBS===" verb-item+

!verb-item :  verb next-verb* s-exp

!verb : ID ["_"]

@next-verb : [!","] verb

everywhere-section : !"===EVERYWHERE===" id-desc+

things-section : !"===THINGS===" thing-item+

!thing-item : DASHED-NAME id-desc+

places-section : !"===PLACES===" place-item+

!place-item : DASHED-NAME STRING place-items id-desc+

!place-items : !"[" ([!","] ID)* !"]"

start-section : !"===START===" ID

!id-desc : ID s-exp

s-exp : ID | STRING | !"(" s-exp* !")"
#lang brag

txtadv-program : verb-section everywhere-section things-section places-section start-section

verb-section : <"===VERBS==="> verb-item+

verb-item : verb-name+ s-exp

verb-name : [<",">] ID ["_"]

everywhere-section : <"===EVERYWHERE==="> id-desc+

things-section : <"===THINGS==="> thing-item+

thing-item : DASHED-NAME id-desc+

places-section : <"===PLACES==="> place-item+

place-item : DASHED-NAME place-descrip place-items id-desc+

place-descrip : STRING ; `place-desc` is already used in expander

place-items : <"["> place-name* <"]"> ; `place-things` is already used

place-name : [<",">] ID

start-section : <"===START==="> place-name

id-desc : ID s-exp

s-exp : ID | STRING | <"("> s-exp* <")">
#lang brag

top : (/NEWLINE+ [line])* /NEWLINE*
line : [lookbehind] pat [lookahead]
pat : repeat+ | choice
lookbehind : /"(" /"?" /"<" /"=" pat /")"
lookahead : /"(" /"?" /"=" pat /")"
choice : pat (/"|" pat)+
repeat : repeatable [("*" | "+") ["?"] | "?"]
@repeatable : group | any | start | end | literals | chars
group : /"(" pat /")"
any : /"."
start : /"^"
end : /"$"
literals : LITERAL+
chars : /"[" LITERAL* /"]"
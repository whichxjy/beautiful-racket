#lang br/ragg

expr : term ('+' term)*
term : factor ('*' factor)*
factor : INT

#lang basic-demo-3
5 def f(x, y) = x * y
10 a = 1 : a = 5 : b = 10
20 gosub 150
30 a = 25
40 gosub 150
50 end
150 print a + a + a
160 return
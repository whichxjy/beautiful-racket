#lang pythonesque-demo

a = 3
b = 4

"middle \" escaped quote"
"ending escaped quote\""
"middle \\ escaped backslash"
"ending escaped backslash\\"

def ft():
  return 42

def gt(x, y):
  return x > y
  def noop():
    return "double dedent here"


def squaresum(x, y):
  def add(c, d):
    return c + d
  return add(x, y) * add(x, y)

gt(a, b) # #f
squaresum(b, a) # 49

println(a)

expt(2, 4)

range(1, 5)

# keep indented example next to eof
for x in range(1, 5):
  println(x * x)

def foo(x):
  x

foo(42) # no return value

if a < b:
  print "a is less than b"
else:
  print "a is not less than b"

def bar(x, y):
  return x > y
  def noop():
    return "double dedent here"
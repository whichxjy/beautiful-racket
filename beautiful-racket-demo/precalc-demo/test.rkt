#lang precalc-demo
fun f(x, y, z) = x + x + x * (y + y) + y * z - z - z

fun g(z) = f(z, z, z) // line comment
g(10) // 300

fun h() = g(10)
h() // also 300

fun k(x) = x / 10 / 10 / (x / x)
k(h()) // 3

36/6/2 // 3

/*
multiline comment
0 / 0 / 0
*/

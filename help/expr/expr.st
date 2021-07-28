'Literals'
0
1
-1
1.2
-1.2
$x
$.
'x'
#'x'
#x
#()
#(1)
#(1.2)
#('x')
#(1 2 3)
#(1.2 3.4 5.6)

'Pseudo Variables'
self "this in SuperCollider"
true
false

'Assignment'
a := 24
x := y
x := 99.9
x := 'y'
x := #y

'Unary Methods'
x y
x y z
x y z y

'Binary Methods'
true = false "== in SuperCollider"
true ~= false "!= in SuperCollider"
4 + 5 = 9 "=> true"
1 ! 2 "=> error"
1 % 2
1 & 2
1 * 2
1 + 2
1 , 2
1 / 2
1 < 2
1 = 2
1 > 2
1 ? 2
1 @ 2
1 \ 2
1 ~ 2
1 | 2
1 - 2
4 + 5 * 6
4 + 5 * 6 / 7

'One Argument Keywords (KeyBinOp)'
(1 min: 2) max: 3 "=> 3"
1 min: (2 max: 3) "=> 3"
1 min: (2 max: 3) "=> 1"
5 min: 2 + 2 "=> 4"
5 min: 2 neg + 6 "=> 4"
(5 min: 2 neg + 6) = 4 "=> true"

'Keyword'
2 max: 3
1 min: 2 max: 3 "=> 3"
1 min: (2 max: 3) "=> 1"

'Unary & Binary & Keyword Methods'
4 + 5 neg
4 + 5 neg + 6
4 neg + 5
1 + 2 * 3 neg
4 + 5 neg "=> -1"
4 + 5 neg = -1 "=> true"
4 + 5 neg < 0 = true "=> true"
4 + 5 neg
w * (x + y) z
w * (x + y) - z
(w * (x + y) z + a) b
(1 + (2 * 3) squared / 4) neg
(2 max: 3) = 3 "=> true"

'Parenthesised Expression'
x y + z
(x y)
(x + y) z
w * (x + y)
(w * (x + y))
(w * (x + y)) z
w * ((x + y) z)
1 + (2 * 3)
(1 + 2 * 3 / 4) neg
(x + y) z
((x + y) z) + a
(x + y) z + a
(((x + y) z) + a) b
((x + y) z + a) b
(1 + (2 * 3)) postln
1 + (2 max: 3) = 4 "=> true"
1 + 2 max: 3 = 4 "=> true"
1 + 2 * 3 max: 4
1@2 = (Point x: 1 y: 2) "=> true"

'Blocks'
[]
[:x| y]
[:x| |y| z]
[SinOsc freq: 440 phase: 0 mul: 0.1]
four = [:x| x + 1] value: 3

'Comments'
x                        "x"
nil                      "()"
[]                       "\n"

'Identifiers'
p
P
pq
p_
p_q
p1

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
#'x y'
#x
#|                       "=> #|"
#()
#(1)
#(1.2)
#('x')
#(1 2 3)
#(1.2 3.4 5.6)
#(#(0 1) #(2 3))

'Pseudo Variables'
self                     "this in SuperCollider"
true
false

'Assignment'
a := 24
x := y
x := 99.9
x := 'y'
x := #y
p := q
p_q := r

'Unary Methods'
x y
x y z
x y z y
P q
#(1 2 3) size            "=> 3"
1 class superclass       "=> Integer"
Object superclass        "=> ProtoObject"
ProtoObject superclass   "=> nil"
nil class                "=> UndefinedObject"
Float negativeInfinity   "=> -Infinity"
SinOsc class class class name = 'Metaclass class'

'Binary Methods'
true = false             "=> false ; == in Sc"
true ~= false            "=> true  ; != in Sc"
1 ~= 2 = true            "=> true"
4 + 5 = 9                "=> true"
p ! q                    "=> error"
p % q                    "=> error"
true & false = false     "=> true"
1 * 2 = 2
1 + 2 = 3
'x','y' = 'xy'           "=> true"
1 / 2                    "=> (1/2)"
1 < 2                    "=> true"
1 = 2                    "=> false"
1 > 2                    "=> false"
p ? q                    "=> error"
1 @ 2                    "=> 1@2"
p \ q                    "=> error"
p ~ q                    "=> error"
true | false             "=> true"
1 - 2                    "=> -1"
4 + 5 * 6
4 + 5 * 6 / 7
2 ** 3 = 8               "=> true"
7 // 3                   "=> 2"
-37.0 / 4.0 = -9.25      "=> true"

'Array Expressions'
{p. q}
{p + q. r s: t. u v}

'Keyword'
2 max: 3                 "=> 3"
1 min: 2 max: 3          "=> 3"
1 min: (2 max: 3)        "=> 1"
(1 min: 2) max: 3        "=> 3"
1 min: (2 max: 3)        "=> 1"
5 min: 2 + 2             "=> 4"
5 min: 2 negated + 6     "=> 4"
2 + 5 min: 5             "=> 5"
2 + (5 min: 5)           "=> 7"
p q: r
nil ifNil: [1]           "=> 1"
SinOsc new perform: #freq: with: 110

'Unary & Binary & Keyword Methods'
4 + 5 neg
4 + 5 neg + 6
4 neg + 5
1 + 2 * 3 neg
4 + 5 neg                "=> -1"
4 + 5 neg = -1           "=> true"
4 + 5 neg < 0 = true     "=> true"
4 + 5 neg
w * (x + y) z
w * (x + y) - z
(w * (x + y) z + a) b
(1 + (2 * 3) squared / 4) neg
(2 max: 3) = 3           "=> true"
(#freq -> 440) class     "=> Association"
1 + -2 abs               "=> 3"
1 + 2 squared negated    "=> -3"

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
1 + (2 max: 3) = 4       "=> true"
1 + 2 max: 3 = 4         "=> true"
1 + 2 * 3 max: 4
1@2 = (Point x: 1 y: 2)  "=> true"
(1 + ((2 * 3) squared) / 4) negated

'Blocks'
[]
[:x| y]
[:x| |y| z]
[SinOsc freq: 440 phase: 0 mul: 0.1]
four = [:x| x + 1] value: 3

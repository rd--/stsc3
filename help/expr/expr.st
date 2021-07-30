'Comments'
x                                                 "x"
nil                                               "()"
[]                                                "\n"

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
1e2                                               "=> 100.0"
1e-2                                              "=> 0.01"
$x                                                "=> $x"
$.                                                "=> $."
$                                                 "=> $ "
'x'
#'x'
#'x y'
#x
#|                                                "=> #|"
#()                                               "=> ()"
#(1)
#(1.2)
#('x')
#(1 2 3)
#(1.2 3.4 5.6)
#(#(0 1) #(2 3))

'Pseudo Variables'
self                                              "this in SuperCollider"
true                                              "=> true"
false                                             "=> true"

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
#(1 2 3) size                                     "=> 3"
1 class superclass                                "=> Integer"
Object superclass                                 "=> ProtoObject"
nil class                                         "=> UndefinedObject"
Float infinity                                    "=> Infinity"
Float negativeInfinity                            "=> -Infinity"
SinOsc class class class name = 'Metaclass class'

'Binary Methods'
true = false                                      "=> false ; == in Sc"
true ~= false                                     "=> true  ; != in Sc"
1 ~= 2 = true                                     "=> true"
4 + 5 = 9                                         "=> true"
p ! q                                             "=> error"
p % q                                             "=> error"
true & false = false                              "=> true"
1 * 2 = 2                                         "=> true"
1 + 2 = 3                                         "=> true"
'x','y' = 'xy'                                    "=> true"
1 / 2                                             "=> 1/2"
1 < 2                                             "=> true"
1 = 2                                             "=> false"
1 > 2                                             "=> false"
p ? q                                             "=> error"
1 @ 2                                             "=> 1@2"
p \ q                                             "=> error"
p ~ q                                             "=> error"
true | false                                      "=> true"
1 - 2                                             "=> -1"
4 + 5 * 6                                         "=> 54"
4 + 5 * 6 / 7                                     "=> 54/7"
2 ** 3 = 8                                        "=> true"
7 // 3                                            "=> 2"
-37.0 / 4.0 = -9.25                               "=> true"

'Array Expressions'
{1. 2}                                            "=> (1 2)"
{1 + 2. 3 min: 4. 5 abs}                          "=> (3 3 5)"
{p. q}
{p + q. r s: t. u v}

'Keyword'
Transcript show: 'text'
1 to: 5 do: [:x| Transcript cr ; show: x]
2 max: 3                                          "=> 3"
1 min: 2 max: 3                                   "=> 3"
1 min: (2 max: 3)                                 "=> 1"
(1 min: 2) max: 3                                 "=> 3"
1 min: (2 max: 3)                                 "=> 1"
5 min: 2 + 2                                      "=> 4"
5 min: 2 negated + 6                              "=> 4"
2 + 5 min: 5                                      "=> 5"
2 + (5 min: 5)                                    "=> 7"
p q: r
nil ifNil: [1]                                    "=> 1"
1 perform: #+ with: 2                             "=> 3"
SinOsc new perform: #freq: with: 110

'Unary & Binary & Keyword Methods'
4 + 5 negated                                     "=> -1"
4 + 5 negated + 6                                 "=> 5"
4 negated + 5                                     "=> 1"
1 + 2 * 3 negated                                 "=> -9"
4 + 5 negated                                     "=> -1"
4 + 5 negated = -1                                "=> true"
4 + 5 negated < 0 = true                          "=> true"
4 + 5 negated                                     "=> -1"
(2 max: 3) = 3                                    "=> true"
1 + -2 abs                                        "=> 3"
1 + 2 squared negated                             "=> -3"
(1 + (2 * 3) squared / 4) negated
(#freq -> 440) class                              "=> Association"
w * (x + y) z
w * (x + y) - z
(w * (x + y) z + a) b

'Parenthesised Expression'
x y + z
(x y)
(x + y) z
w * (x + y)
(w * (x + y))
(w * (x + y)) z
w * ((x + y) z)
1 + (2 * 3)                                       "=> 7"
(1 + 2 * 3 / 4) negated                           "=> 7"
(x + y) z
((x + y) z) + a
(x + y) z + a
(((x + y) z) + a) b
((x + y) z + a) b
(1 + (2 * 3)) postln
1 + (2 max: 3) = 4                                "=> true"
(1 + 2 max: 3) = 3                                "=> true"
1 + 2 * 3 max: 4                                  "=> 9"
1@2 = (Point x: 1 y: 2)                           "=> true"
(1 + ((2 * 3) squared) / 4) negated

'Blocks'
[]
[:x| y]
[:x| |y| z]
[SinOsc freq: 440 phase: 0 mul: 0.1]
four := [:x| x + 1] value: 3

'Initializer Definition'
|a b| a := 1 . ^ a + b .
SinOsc freq: (69 midicps) phase: 0 mul: 0.1
SinOsc freq: (69 midicps) phase: 0 mul: 0.1
|a b c| a := 1 . b := 2 . c := 3 . ^ a + b + c .
|a b c| a := [1] . b := [2] . c := [3] . ^ a value + b value + c value .
[:x | x * x] value: Float pi * 2

'Block Constructor'
[1]
[^1]
[:a| |t|]
[:a| |t u| x . y]
[:a| |t u| t := [:b| |v w| z] . x . y]

'Statements'
^p
^p.
p
p q
(p q)
p. ^q
^ 440 * (2 ** ((self - 69) * (1 / 12)))
p. q.

'Return Statement'
^1
^p
^a value + b value + c value
^self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]

'Method block body'
440 * (2 ** ((self - 69) * (1 / 12)))

'Expression'
p := 1
p := q
p
1
1 + 2
p := (1 + 2)
p := (1 + 2) negate
a value + b value + c value
note2 := (sequR value: #(-12 -7 -5 0 2 5) value: clock_16) + note1
freq := ((#(-2 0 3 5 7 10 12 15) at: i) + 60) midicps .
(1 to: 6) do: [:i| s := u in: s maxdelaytime: 0.1 delaytime: {0.05 rand . 0.05 rand} decaytime: 4] .
out := pitch size mixFill: [:i| |t| t := HPZ1 in: (mousex > (0.25 + ((i - 1) * spacing))) abs . x]
out := pitch size mixFill: [:i| x]
pitch size mixFill: x
[1] value
o m: true
self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]

'Assignment'
p := 1
p := q
p := 2.0
p := 'x'
p := 8 mixFill: [:i| |n trigger pluck freq z metal| n := 15 ]
p := q := r := nil

'Basic Expression'
1
p
1 negate
p negate
1 + 2
(p)
(p q)
a value + b value + c value
p q r: x
self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]
self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]
w * ((x + y) z)
w * (x + y) z
Transcript show: 'hello '; show: 'world'; cr.

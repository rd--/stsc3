'Comments'

x                                                 "x"
nil                                               "()"
[]                                                "\n '"

'Identifiers'

p
P
pq
p_
p_q
p1

'Literals'

0                                                 "=> integer"
1                                                 "=> integer"
-1                                                "=> negative integer"
1.2                                               "=> float"
-1.2                                              "=> negative float"
1e2                                               "=> float with exponent 100.0"
1e-2                                              "=> float with negative exponent 0.01"
$x                                                "=> character $x"
$.                                                "=> $."
$                                                 "=> $ "
'x'                                               "=> string"
#'x'                                              "=> hashquoted string (symbol)"
#'x y'                                            "=> symbol with space character"
#x                                                "=> unary selector literal"
#x = #'x'                                         "=> true"
#x:                                               "=> keyword selector literal"
#x: = #'x:'                                       "=> true"
#+                                                "=> binary selector literal"
#+ = #'+'                                         "=> true"
#|                                                "=> #|"
#| = #'|'                                         "=> true"
#()                                               "=> empty literal array ; ()"
#() = #()                                         "=> true"
#(1)
#(1.2)
#('x')
#(#'x')
#(#x) = #(#'x')                                   "=> true"
#(1 2 3)
#(1.2 3.4 5.6)
#(#(0 1) #(2 3))

'Pseudo Variables'

self                                              "=> nil ; this in SuperCollider"
super                                             "=> nil"
true                                              "=> true"
false                                             "=> false"
nil                                               "=> nil"

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
7 rem: 3                                          "=> 1"
1.5 // 0.5                                        "=> 3"
1.5 rem: 0.5                                      "=> 0.0"
-37.0 / 4.0 = -9.25                               "=> true"
$c class = Character                              "=> true"

'Array Expressions'

{1. 2}                                            "=> (1 2)"
{1 + 2. 3 min: 4. 5 abs}                          "=> (3 3 5)"
{p. q}
{p + q. r s: t. u v}

'Interval'
(1 to: 9 by: 2) asArray                           "=> #(1 3 5 7 9)"
(1 to: 9 by: 2) + 1                               "=> (2 to: 10 by: 2)"
1 + (1 to: 9 by: 2)                               "=> #(2 4 6 8 10)"
(1 to: 9 by: 2) * 2                               "=> #(2 6 10 14 18)"
(9 to: 1) asArray                                 "=> #()"
(9 to: 1 by: -1) asArray                          "=> #(9 8 7 6 5 4 3 2 1)"
(1 to: 9 by: 2) collect: [:a | a + a]             "=> #(2 6 10 14 18)"
(0.5 to: 4.5 by: 0.5) asArray                     "=> #(0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5)"
(1 to: 2) sin                                     "=> #(0.8414709848078965 0.9092974268256817)"
9 iota                                            "=> #(1 2 3 4 5 6 7 8 9)"
5 from: 1 by: 2                                   "=> #(1 3 5 7 9)"
(9 from: 1 by: 1) = 9 iota                        "=> true"

'Keyword'

Transcript show: 'text'                           "=> prints text (no newline)"
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
[] = []                                           "=> false"
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
[[^0] value. Transcript show: 'x'] value

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
out := (1 to: pitch size) mixFill: [:i| |t| t := HPZ1 in: (mousex > (0.25 + ((i - 1) * spacing))) abs . x]
out := pitch size mixFill: [ x ]
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

'Unary Messages'

abs
abs square
abs square + p
abs square + p + q
value + b value + c value
abs square + p + q k: r
abs square + p + q k1: p1 k2: p2
q r: x

'Cascaded Messages'

p q ; r ; s ; t
(1 + 3 ; + 3) = 4                                 "=> true"
Transcript show: 'hello'; show: 'world'; cr.      "=> prints hello then world then carriage return"
Transcript cr ; show: 'newline'; cr.              "=> prints newline on a newline"
Transcript show: (194 * 9) printString; cr.       "=> prints 1746 then a cr"
Point new setX: 25 setY: 35; isZero               "=> false"
1 to: 5 do: [:x| Transcript cr ; show: x]
OrderedCollection new add: 1 ; add: 2 ; yourself  "=> adds 1 and then 2 to the new collection"

'Integer'

1 to: 5 do: [ :x | Transcript show: x asString ]            "=> print 1 to 5"
1 to: 10 do: [ :x | Transcript show: 5 atRandom asString ]  "=> print 10 random numbers in (1,5)"

'Number'

9 \\ 4 = 1                                        "=> true ; modulo ; sc>>%"
-9 \\ 4 = 3                                       "=> true"
9 \\ -4 = -3                                      "=> true"
0.9 \\ 0.4                                        "=> ~ 0.1"

'String'

'quoted''string'
'text' isLetters = true                           "=> true"
'0123' isDigits = true                            "=> true"
'    ' isWhiteSpace = true                        "=> true"
('string' substringFrom: 2 to: 4) = 'tri'         "=> true"
'\\' size = 2                                     "=> true"
'\0' = '\0'                                       "=> true"
#'x' = 'x'                                        "=> true"
#'x' hashcode = 'x' hashcode                      "=> true"

'Symbol'

#'quoted''symbol'

'Vector'

((1,2,3) instVarAt: 2 put: 4) = 4                 "=> true"

'Reflection'

Array methods inspect
(Array methods at: 1)
(Array methods at: 1) invokeOn: #(1 2 3) with: #(2)

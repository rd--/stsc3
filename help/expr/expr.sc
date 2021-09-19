'Comments'
x // y

'Literals'
0
1
-1
1.2
-1.2
inf
$x
$.
$$
"x"
'x'
\x

'Literal Arrays'
#[]
#[1]
#[1.2]
#["x"]
#['x']
#[\x]
#[1,2,3]
#[1.2,3.4,5.6]

'Pseudo Variables'
pi
true
false
this // self in Smalltalk

'Assignment'
a = 24
x = y
x = 99.9
x = \y
x = 'y'

'Unary Methods'
x.y
x.y.z
x.y.z.y

'Binary Methods'
4 + 5 == 9                                        // true
1 ! 2 == [1,1]                                    // true
1 @ 2 == Point.new(x: 1, y: 2)                    // true
1 @ 2
1 % 2 == 1                                        // true
1.5 % 0.5 == 0                                    // true
(5.iota % 2) == [0,1,0,1,0]                       // true
1 & 2
1 * 2
1 + 2
1 < 2
1 > 2
1 ? 2
1 / 2
1 - 2
1 | 2
1 <> 2
1 == 2
4 + 5 * 6
4 + 5 * 6 / 7
1.2 ** i / 16
4**LFNoise2.kr(freq:1.2**i/16)
5**LFNoise2.ar(f)/(i+8)*20
x*(5**LFNoise2.ar(f)/(i+8)*20)
p + (q + r.s())
{var d = 1 / p.q(x: a + r.s); d}
{var d = 1 / p.q(x: a + r.s()); d}

'Array Expression'
[]
[1,2.3]
[true,false,nil]
1 ! 2 == [1,1] // true

'Keyword'
2.max(3)                                // 3
2.max(3) + 4                            // 7
1.min(2).max(3)                         // 3
1.min(2.max(3))                         // 1
1.min(2.max(3))                         // 1
2.max(aNumber: 3) == 3                  // true
SinOsc.ar()
SinOsc.ar(440)
SinOsc.ar(440, 0)
SinOsc.ar(freq: 440)
SinOsc.ar(phase: 0)
SinOsc.ar(freq: 440, phase: 0)
SinOsc.ar(440, phase: 0)
SinOsc.ar((60 + 9),0)
SinOsc.ar((60.midicps),0)
x.y(a).z(b)
p.q(r).s
Signal.hanningWindow(1024)
Signal.hanningWindow(1024).squared

'Unary & Binary & Keyword Methods'
4 + 5.neg
4 + 5.neg + 6
4.neg + 5
1 + 2 * 3.neg
4 + 5 . neg                             // -1
4 + 5 . neg == -1                       // true
4 + 5 . neg < 0 == true                 // true
4 + 5.neg
w * (x + y).z
w * (x + y) - z
(w * (x + y).z + a).b
(1 + (2 * 3).squared / 4).neg == -9.25
1 + -2.abs.min(3).squared
2.max(3) == 3                           // true
1 + 2 . max(3) == 4                     // true
SinOsc.ar(60.midicps)
SinOsc.ar(60.midicps,0)
SinOsc.ar(60.midicps,pi)
SinOsc.ar(60.midicps,pi / 4)
SinOsc.ar(60 + 9,0)
SinOsc.ar(60.midicps,0)

'Parenthesised Expressions'
x.y + z
(x.y)
(x + y).z
w * (x + y)
(w * (x + y))
(w * (x + y)).z
w * ((x + y).z)
1 + (2 * 3)                        // 7
(1 + 2 * 3 / 4).neg                // -2.25
(x + y).z
((x + y).z) + a
(x + y).z + a
(((x + y).z) + a).b
((x + y).z + a).b
(1 + (2 * 3)).postln
1 + (2 . max(3)) == 4              // true
1 + 2 . max(3) == 4                // true
1 + 2 * 3 . max(4) == 12           // true
1 @ 2 == Point.new(x: 1, y: 2)     // true

'Blocks'
{}
{arg x; y}
{arg x; x}
{arg x; var y; z}
{SinOsc.ar(mul: 0.1)}.play
{arg x; x + 1}.value(3) == 4            // true
{^nil}.value                            // return in non-method
f = {arg x=1, y; [x,y]}
f.value == [1,nil]
f.value() == [1,nil]
f.value(3) == [3,nil]
f.value(x:3) == [3,nil]
f.value(3,2,1) == [3,2]
f.value(z:1,x:3) == [3,nil]
f = {arg x, y, z; [x,y,z]}
f.value(1,2,3,4,5,6,z:7,y:8,x:9) == [9,8,7] // true
[1,2,3].collect({'x'.postln}) == ['x','x','x']
[1,2,3].collect({arg i; i * i}) == [1,4,9]
[1,2,3].collect({arg i, j; i * j}) == [2,4,6]
[1,2,3].collect({arg i, j, k = 3; i * j * k}) == [0,6,18]

'Methods'
-3.abs == 3
-3.abs() == 3
-3.abs(1) == 3
-3.abs(x:0) == 0
-3.performList(selector: 'abs', arglist: [1,2,3]) == 3

'Temporaries'
{var x; }
{var x, y; x}
{var x; x = p.q(r)}
{var x = p.q(r); }
{arg x; var y; nil}
{arg x; var y, z; nil}
{arg x; var y, z; y = z = x; x * y * z}
{arg x; var y = 0; nil}
{arg x; var y = x.abs; nil}
{arg x; var y, z = y; x * y * z}
{arg x; var y; var z = y; nil}

'Precedence Rules'
1                        // => 1
p.q                      // => p q
p.q + r                  // => p q + r
p + q.r                  // => p + q.r
p + q.r()                // => p + (q.r()) ==> p + (q r: ({}))
p + q.r(x + y.z())       // => p + (q.r(x + (y.z())))
p + q + r.s()            // => p + q + (r.s())
p + q + r.s() + t        // => p + q + (r.s()) + t
p + (q + r.s())          // => p + (q + (r.s()))
p + q.r().s              // => p + (q.r()).s
p + q.r.s()              // => p + (q r s: ({}))
p + q.r.s().t            // => p + (q r s: ({})) t
p + q.r.s().t.u()        // => p + ((q r s: ({})) t u: ({}))
p.q()                    // => p.q() ==> p q: ({})
p.q().r                  // => (p.q()).r ==> (p q: ({})) r
p.q().r + s              // => (p.q()).r + s
p.q().r()                // => (p q: ({})) r: ({})
p.q.r()                  // => (p.q.r())
p + q                    // => p + q
p.q + r                  // => p q + r
p.q() + r                // => (p q: ({})) + r
p.q() + r.s( )           // => (p q: ({})) + (r s: ({}))
{p.q().r}                // => [ (p q: ({})) r .\n ]
{var x = p.q().r; x}     // => [ |x|\n x := (p q: ({})) r .\n x .\n ]
1 + p.q(x: a + r.s())    // => 1 + (p.q(x:a + (r.s())))
p.q(r + s.t())           // => p.q(r + (s.t()))
p.q(a + b + c * d).r * s // => (p.q(a + b + c * d)).r * s
p.q(r.s(x.y()))          // => p.q(r.s(x.y()))
p.q(r.s(x.y.z))          // => p.q(r.s(x.y.z))
r.s(x.y.z())             // => r.s((x.y.z()))
p.q(r.s(x.y.z()))        // => p.q(r.s((x.y.z())))
p + q.r()                // => p + (q.r())
p + q.r.s()
p + q.r.s().t            // => p + ((q.r.s())).t
p + q.r.s().t()
p + q.r.s().t.u()
p.q(a + r.s())           // => p.q(a + (r.s()))
1 / p.q(a + r.s())
1 / p.q(x: a + r.s())

'Temporaries (Variable Declarations)'
{var x = a; x}           // {var x; x = a; x}
{var x = a,y; x + y}     // {var x,y; x = a; x + y}
{var x;var y = b;x + y}  // {var x,y; y = b; x + y}
{var x={var y=a;a*x}; x} // {var x; x = {var y; y = a; a * x}; x}
{arg a; var p = x; var q = y; x + y}
{u.v; {arg a; var p = x; var q = y; x + y}}
{u.v(m: {arg a; var p = x; var q = y; x + y})}

'Keyword Arguments (Parameters)'
p.q()                    // p.q([])
p.q(a)                   // p.q([a])
p.q(a,b)                 // p.q([a,b])
p.q(x:a,b)               // p.q([\x: -> (a),b])
p.q(a,x:b)               // p.q([a,\x: -> (b)])
p.q(a,x: b.c(y: d,e))    // p.q([a,\x: -> (b.c([\y: -> (d),e]))])

'Boolean'
1 != 2 == true                // => true
true.and(false) == false      // => true
true.or(false) == true        // => true
true || false == true         // => true
true && false == false        // => true


'Strings'
"x" + "y" == "x y"            // => true
"x" ++ "y" == "xy"            // => true
"".isEmpty == true            // true
"text".copyRange(2,2) == "x"  // true
"text".copyRange(1,2) == "ex" // true

'Symbols'
'x 1'.asString == "x 1"            // true
'x $ x'.asString == "x $ x"        // true
'x y'.class == Symbol              // true
'x y'.asCompileString == "'x y'"   // true
\x.class == Symbol                 // true
\x.asCompileString == "'x'"        // true

'Array'
Array.new(6).size == 0        // => true
Array.newClear(6).size == 6   // => true

'Integer Math'
5.div(2) == 2                 // => true
7.div(3) == 2                 // => true
-5.div(2) == -3               // => true
-7.div(3) == -3               // => true

'Complex'
Complex.new(-1,0).pow(1/2)

'Classes'
SinOsc.name == 'SinOsc'                 // true
SinOsc.class.class == Class             // true
SinOsc.superclass == PureUGen           // true
SinOsc.superclass.superclass == UGen    // true
SinOsc.subclasses == nil                // true
UGen.subclasses.size == 325             // true
SinOsc.superclasses == [PureUGen, UGen, AbstractFunction, Object] // true

'Iteration'
4.for(5,{arg x; [\for,x].postln})
5.for(5,{arg x; [\for,x].postln})
6.for(5,{arg x; [\for,x].postln})
6.0.for(5.0,{arg x; [\for,x].postln})
6.forBy(5,-1, {arg x; [\forBy,x].postln})
6.0.forBy(5.0,-1.0, {arg x; [\forBy,x].postln})

'Randomness'
{var i=0,x=nil; {x=1.0.rand; i=i+1; x>0.1}.while({[x,i].postln})}.value

'Bitwise'
0 | 1 == 1               // true

'Variable Arguments'
-3.abs == 3
-3.abs() == 3
-3.abs(nil) == 3

'Keyword operators'
3 min: 2                 // 2
3 min: 2 + 1             // 3
3 min: -2.abs + 1        // 3
3 min: 2.max(1)          // 2
3 max: 2 . squared       // 4
3 max: (2 . squared)     // 4
(3 max: 2) . squared     // 9
2 min: 3 pow: 2          // 4.0
2 min: 3.pow(2)          // 2.0
(2 min: 3) pow: 2        // 4.0
2 min: (3 pow: 2)        // 2.0
1.min(2).max(3) == 3     // true
1.min(2.max(3)) == 1     // true
1 min: 2 max: 3 == 3     // true
1 min: (2 max: s) == 1   // true
5 min: 2 + 2 == 4        // true (keyword operator binds as keyword message in smalltalk)
5 min: 2 . neg + 6 == 4  // true

'Interval'
1.to(9) + 1 == Interval.new(2, 10, 1)             // true
1.to(9) - 1 == Interval.new(0, 8, 1)              // true
1.to(9) * 3 == Interval.new(3, 27, 3)             // true
(1.to(9) * 3).asArray == 3.series(6,27)           // true ; (3,6 .. 27)
(0.to(9) * 3).at(5) == 15                         // true
0.5.to(4.5,0.5) == Interval.new(0.5, 4.5, 0.5)    // true
1.to(1e15)                                        // Interval
1.to(9).collect({arg i; i + i})                   // [2, 4, 6, 8, 10, 12, 14, 16, 18]
1.to(9,2).collect({arg i; i + i})                 // [ 2, 6, 10, 14, 18 ]
9.to(1,-1).asArray == 9.series(nil,1)             // true ; (9 .. 1)
9.to(1).asArray == []                             // true

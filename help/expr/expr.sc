'Binary Methods'
4 + 5 * 6
4 + 5 * 6 / 7
1.2 ** i / 16
5**LFNoise2.ar(f)/(i+8)*20
x*(5**LFNoise2.ar(f)/(i+8)*20)
p + (q + r.s)

'Array Expression'
[]
[1,2.3]
[true,false,nil]
1 ! 2 == [1,1] // true

'N-ary messages'
2.max(3)                                // 3
2.max(3) + 4                            // 7
1.min(2).max(3)                         // 3
1.min(2.max(3))                         // 1
1.min(2.max(3))                         // 1
SinOsc.ar
SinOsc.ar(440)
SinOsc.ar(440, 0)
SinOsc.ar((60 + 9),0)
SinOsc.ar((60.midiCps),0)
x.y(a).z(b)
p.q(r).s
Signal.hanningWindow(1024)
Signal.hanningWindow(1024).squared

'Unary & Binary & N-ary Methods'
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
SinOsc.ar(60.midiCps)
SinOsc.ar(60.midiCps,0)
SinOsc.ar(60.midiCps,pi)
SinOsc.ar(60.midiCps,pi / 4)
SinOsc.ar(60 + 9,0)
SinOsc.ar(60.midiCps,0)

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

'Blocks'
{}
{arg x; y}
{arg x; x}
{arg x; var y; z}
{arg x; x + 1}.value(3) == 4            // true
{^nil}.value                            // return in non-method
f = {arg x=1, y; [x,y]}
f.value == [1,nil]
f.value(3) == [3,nil]
f.value(3,2,1) == [3,2]
f = {arg x, y, z; [x,y,z]}
[1,2,3].collect({'x'.postln}) == ['x','x','x']
[1,2,3].collect({arg i; i * i}) == [1,4,9]
[1,2,3].collect({arg i, j; i * j}) == [2,4,6]
[1,2,3].collect({arg i, j, k = 3; i * j * k}) == [0,6,18]

'Methods'
-3.abs == 3
-3.abs(1) == 3

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
p + q                    // => p + q
p.q + r                  // => p q + r
p.q(a + b + c * d).r * s // => (p.q(a + b + c * d)).r * s
p.q(r.s(x.y.z))          // => p.q(r.s(x.y.z))

'Temporaries (Variable Declarations)'
{var x = a; x}           // {var x; x = a; x}
{var x = a,y; x + y}     // {var x,y; x = a; x + y}
{var x;var y = b;x + y}  // {var x,y; y = b; x + y}
{var x={var y=a;a * x}; x} // {var x; x = {var y; y = a; a * x}; x}
{arg a; var p = x; var q = y; x + y}
{u.v; {arg a; var p = x; var q = y; x + y}}

'N-ary Arguments (Parameters)'
p.q(a)                   // p.q([a])
p.q(a,b)                 // p.q([a,b])

'Integer Math'
5.div(2) == 2                 // => true
7.div(3) == 2                 // => true
-5.div(2) == -3               // => true
-7.div(3) == -3               // => true

'Iteration'
4.for(5,{arg x; [\for,x].postln})
5.for(5,{arg x; [\for,x].postln})
6.for(5,{arg x; [\for,x].postln})
6.0.for(5.0,{arg x; [\for,x].postln})
6.forBy(5,-1, {arg x; [\forBy,x].postln})
6.0.forBy(5.0,-1.0, {arg x; [\forBy,x].postln})

'Randomness'
{var i=0,x=nil; {x=1.rand; i=i+1; x>0.1}.while({[x,i].postln})}.value

'Variable Arguments'
-3.abs == 3
-3.abs(nil) == 3

'Interval'
1.to(9) + 1 == Interval.new(2, 10, 1)             // true
1.to(9) - 1 == Interval.new(0, 8, 1)              // true
1.to(9) * 3 == Interval.new(3, 27, 3)             // true
(1.to(9) * 3).asArray == 3.series(6,27)           // true ; (3,6 .. 27)
(0.to(9) * 3).at(5) == 15                         // true
0.5.to(4.5,0.5) == Interval.new(0.5, 4.5, 0.5)    // true
1.to(10 ** 15)                                        // Interval
1.to(9).collect({arg i; i + i})                   // [2, 4, 6, 8, 10, 12, 14, 16, 18]
1.to(9,2).collect({arg i; i + i})                 // [ 2, 6, 10, 14, 18 ]
9.to(1,-1).asArray == 9.series(nil,1)             // true ; (9 .. 1)
9.to(1).asArray == []                             // true

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
4 + 5 == 9 // true
1 ! 2 == [1,1] // true
1 @ 2
1 % 2
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
2.max(3)        // 3
2.max(3) + 4    // 7
1.min(2).max(3) // 3
1.min(2.max(3)) // 1
1.min(2.max(3)) // 1
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
4 + 5 . neg // -1
4 + 5 . neg == -1 // true
4 + 5 . neg < 0 == true // true
4 + 5.neg
w * (x + y).z
w * (x + y) - z
(w * (x + y).z + a).b
(1 + (2 * 3).squared / 4).neg
1 + -2.abs.min(3).squared
2.max(3) == 3 // true
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
1 + (2 * 3)
(1 + 2 * 3 / 4).neg
(x + y).z
((x + y).z) + a
(x + y).z + a
(((x + y).z) + a).b
((x + y).z + a).b
(1 + (2 * 3)).postln
1 + (2 . max(3)) == 4 // true
1 + 2 . max(3) == 4 // true
1 + 2 * 3 . max(4)
1 @ 2 == Point.new(x: 1, y: 2) // true

'Blocks'
{}
{arg x; y}
{arg x; x}
{arg x; var y; z}
{SinOsc.ar(mul: 0.1)}.play
four = {arg x; x + 1}.value(3)

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
p + q.r()                // => p + (q r: ({}))
p + q.r(x + y.z())       // => p + (q.r(x + (y.z())))
p + q + r.s()            // => p + q + (r.s())
p + q + r.s() + t        // => p + q + (r.s()) + t
p + (q + r.s())
p + q.r.s()              // => p + (q r s: ({}))
p + q.r.s().t            // => p + (q r s: ({})) t
nil // p + q.r.s().t.u() // => p + ((q r s: ({})) t u: ({}))
p.q()                    // => p q: ({})
p.q().r                  // => (p q: ({})) r
p.q().r()                // => (p q: ({})) r: ({})
p + q                    // => p + q
p.q + r                  // => p q + r
p.q() + r                // => (p q: ({})) + r
p.q() + r.s( )           // => (p q: ({})) + (r s: ({}))
{p.q().r}                // => [ (p q: ({})) r .\n ]
{var x = p.q().r; x}     // => [ |x|\n x := (p q: ({})) r .\n x .\n ]
1 + p.q(x: a + r.s())    // => 1 + (p.q(x:a + (r.s())))

'Temporaries (Variable Declarations)'
{var x = a; x}           // {var x; x = a; x}
{var x = a,y; x + y}     // {var x,y; x = a; x + y}
{var x;var y = b;x + y}  // {var x,y; y = b; x + y}
{var x={var y=a;a*x}; x} // {var x; x = {var y; y = a; a * x}; x}

'Keyword Arguments (Parameters)'
p.q()                    // p.q([])
p.q(a)                   // p.q([a])
p.q(a,b)                 // p.q([a,b])
p.q(x:a,b)               // p.q([\x: -> (a),b])
p.q(a,x:b)               // p.q([a,\x: -> (b)])
p.q(a,x: b.c(y: d,e))    // p.q([a,\x: -> (b.c([\y: -> (d),e]))])

'Variable Arguments'
-3.abs == 3
-3.abs() == 3
-3.abs(nil) == 3

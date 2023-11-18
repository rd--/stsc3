(* Rand *)
SinOsc(Rand(220, 440), 0) * 0.1

(* Rand *)
SinOsc(
	{ Rand(220, 440) } ! 7,
	0
).Splay * 0.1

(* Rand *)
var f1 = Rand(220, 600);
var f2 = Rand(220, 600);
SinOsc([f1, f2], 0) * 0.1

(* Rand *)
EqPan(PinkNoise(), Rand(-1, 1)) / 10

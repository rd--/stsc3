(* DustRange ; a = noise, b = impulses *)
var a = DustRange(0.0001, 0.001) * 0.1;
var b = DustRange(0.1, 0.1) * 0.5;
[a, b]

(* DustRange ; audio rate impulse train of varying amplitude (min = max) *)
[
	DustRange(0.1, 0.1),
	Impulse(10, 0)
] * 0.1

(* DustRange ; mean iot=0.0005 is equivalent to density=2000 at dust, dustRange spacing is more uniform *)
[
	DustRange(0.0001, 0.001) * 0.1,
	Dust(2000) * 0.05
]

(* DustRange ; velvet noise (approx.) *)
var iot = 20 / SampleRate();
var x = MouseX(1, 16, 1, 0.1);
var d = DustRange(iot / x, iot * x);
var s = Rand(d, -1, 1);
Trig(d, SampleDur()) * s.Sign * 0.1

(* ---- notes.md ---- *)
DustRange ; inter-offset times generated randomly in range (seconds) ; uniform distribution

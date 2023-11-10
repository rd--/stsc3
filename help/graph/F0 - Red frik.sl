(* red frik (f0) ; requires=kr *)
var tr = Impulse(0.1, 0);
var o1 = MulAdd(
	SinOsc(Rand(tr, 0.3, 5), 0),
	Rand(tr, 0, 0.5),
	Rand(tr, 0.49, 0.56)
);
var o2 = MulAdd(
	SinOsc(o1, 0),
	Rand(tr, 0.3, 0.6),
	Rand(tr, 0.3, 0.5)
);
Rhpf(
	{ BrownNoise() } ! 2,
	Rand(tr, 0.3, 3).kr,
	o2.kr
) * 0.1

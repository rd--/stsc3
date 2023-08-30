(* DelayC ; decorrelation *)
var x = BrownNoise() * 0.2;
[
	DelayC(x, 0.01, 0.01),
	DelayC(x, 0.02, MouseX(0.02, 0, 0, 0.2))
]

(* DelayC ; inter-speaker delays *)
var x = LfTri(1000, 0) * Decay2(Impulse(4, 0) * 0.2, 0.004, 0.2);
[
	DelayC(x, 0.01, 0.01),
	DelayC(x, 0.02, MouseX(0.02, 0, 0, 0.2))
]

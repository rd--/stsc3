(* 20061023 ; rd *)
var tr = Impulse(
	Slope(SinOsc(LfNoise0([0.5, 1.5]), 0)).Abs * [2, 3],
	0
);
Ringz(
	Decay2(tr, 0.1, 0.2),
	TrRand(tr, MouseX(960, 3620, 1, 0.2), 3940),
	TrRand(tr, 0.005, 0.275) * MouseY(0.5, 2.0, 0, 0.2)
)

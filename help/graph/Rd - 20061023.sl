(* 20061023 ; rd *)
var t = Impulse(
	Slope(SinOsc(LfNoise0([0.5, 1.5]), 0)).Abs * [2, 3],
	0
);
Ringz(
	Decay2(t, 0.1, 0.2),
	TRand(MouseX(960, 3620, 1, 0.2), 3940, t),
	TRand(0.005, 0.275, t) * MouseY(0.5, 2.0, 0, 0.2)
)

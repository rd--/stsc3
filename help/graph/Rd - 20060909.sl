(* 20060909 ; rd *)
var p = {
	Resonz(
		Pulse(
			LfNoise0(4) * [32, 64],
			LfNoise0(32) * MouseX(0.001, 0.02, 1, 0.1)
		).Lpz2,
		LfNoise0(2) * 0.1 + MouseY(120, 400, 1, 0.1),
		(LfNoise0(6) * 0.4) + 0.8
	) * 0.5
};
var q = {
	CombN(p(), 0.2, LfNoise0(128) * 0.1 + 0.1, 3)
};
var r = {
	{
		{
			SinOsc(
				Rand(50, 59) * MouseX(0.75, 1.25, 1, 0.1),
				0
			) * Rand(0.04, 0.06) * MouseY(0.25, 1, 1, 0.1)
		} !+ 16
	} ! 2
};
p() + q() + r()

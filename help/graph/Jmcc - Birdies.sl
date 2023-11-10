(* birdies (jmcc) #6 *)
{ :tr |
	var p1 = {
		LfPulse(
			Rand(tr, 0.4, 1.4),
			0,
			Rand(tr, 0.1, 0.9)
		) * Rand(tr, 4, 7)
	};
	var p2 = LfPulse(Rand(tr, 0.2, 0.7), 0, 0.4) * 0.02;
	var sw = MulAdd(
		LfSaw(p1() + p1() + 2, 0),
		Rand(tr, 1000, 1800).Neg,
		4000 + Rand(tr, -1200, 1200)
	);
	var freq = Lag(sw, 0.05);
	var amp = Lag(p2, 0.3);
	EqPan2(
		SinOsc(freq, 0) * amp,
		Rand(tr, -1, 1)
	)
}.OverlapTexture(7, 4, 4)

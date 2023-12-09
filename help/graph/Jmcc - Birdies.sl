(* birdies (jmcc) #6 *)
{ :tr |
	var p1 = {
		LfPulse(
			TRand(0.4, 1.4, tr),
			0,
			TRand(0.1, 0.9, tr)
		) * TRand(4, 7, tr)
	};
	var p2 = LfPulse(TRand(0.2, 0.7, tr), 0, 0.4) * 0.02;
	var sw = MulAdd(
		LfSaw(p1() + p1() + 2, 0),
		TRand(1000, 1800, tr).Neg,
		4000 + TRand(-1200, 1200, tr)
	);
	var freq = Lag(sw, 0.05);
	var amp = Lag(p2, 0.3);
	EqPan2(
		SinOsc(freq, 0) * amp,
		TRand(-1, 1, tr)
	)
}.OverlapTexture(7, 4, 4).Mix

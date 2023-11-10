(* pond life (jmcc) #1 ; graph rewrite ; requires=LinRand *)
{ :tr |
	var f = MulAdd(
		SinOsc(Rand(tr, 20, 50), 0),
		Rand(tr, 100, 400),
		LinRand(tr, 500, 2500, 0)
	);
	var e = LfPulse(3 / Rand(tr, 1, 9), 0, Rand(tr, 0.2, 0.5));
	EqPan2(
		SinOsc(f, 0) * e * 0.04,
		Rand(tr, -1, 1)
	)
}.OverlapTexture(8, 8, 4)

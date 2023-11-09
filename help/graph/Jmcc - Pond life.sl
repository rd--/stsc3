(* pond life (jmcc) #1 ; graph rewrite ; requires=TrLinRand *)
{ :tr |
	var f = MulAdd(
		SinOsc(TrRand(tr, 20, 50), 0),
		TrRand(tr, 100, 400),
		TrLinRand(tr, 500, 2500, 0)
	);
	var e = LfPulse(3 / TrRand(tr, 1, 9), 0, TrRand(tr, 0.2, 0.5));
	EqPan2(
		SinOsc(f, 0) * e * 0.04,
		TrRand(tr, -1, 1)
	)
}.OverlapTexture(8, 8, 4)

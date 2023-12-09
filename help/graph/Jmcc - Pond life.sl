(* pond life (jmcc) #1 ; graph rewrite ; requires=LinRand *)
{ :tr |
	var f = MulAdd(
		SinOsc(TRand(20, 50, tr), 0),
		TRand(100, 400, tr),
		TLinRand(500, 2500, 0, tr)
	);
	var e = LfPulse(3 / TRand(1, 9, tr), 0, TRand(0.2, 0.5, tr));
	EqPan2(
		SinOsc(f, 0) * e * 0.04,
		TRand(-1, 1, tr)
	)
}.OverlapTexture(8, 8, 4).Mix

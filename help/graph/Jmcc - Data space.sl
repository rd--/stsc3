(* data space (jmcc) #2 ; graph rewrite *)
{ :tr |
	var dt = TrRand(tr, 0, 0.25) + 0.1;
	var osc = { :n :m |
		var e = MulAdd(
			LfPulse(TrRand(tr, 0, m), 0, TrRand(tr, 0, 1)),
			TrRand(tr, 0, 8000),
			TrRand(tr, 0, 2000)
		);
		LfPulse(TrRand(tr, 0, n), 0, TrRand(tr, 0, 1)) * e
	};
	var freq = osc(200, 40) + osc(20, 4) + osc(20, 4);
	CombL(
		EqPan2(LfPulse(freq, 0, 0.5), LfNoise0(TrRand(tr, 0, 3)) * 0.8),
		dt,
		dt,
		3
	) * 0.04
}.OverlapTexture(6, 1, 4)

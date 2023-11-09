(* uplink (jmcc) #2 ; graph rewrite *)
{ :tr |
	var osc = {
		var e = MulAdd(
			LfPulse(TrRand(tr, 0, 4), 0, TrRand(tr, 0, 1)),
			TrRand(tr, 0, 8000),
			TrRand(tr, 0, 2000)
		);
		LfPulse(TrRand(tr, 0, 20), 0, TrRand(tr, 0, 1)) * e
	};
	EqPan2(
		LfPulse(osc() + osc(), 0, 0.5) * 0.04,
		TrRand(tr, 0, 0.8)
	)
}.OverlapTexture(4, 1, 5)

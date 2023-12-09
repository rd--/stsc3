(* uplink (jmcc) #2 ; graph rewrite *)
{ :tr |
	var osc = {
		var e = MulAdd(
			LfPulse(TRand(0, 4, tr), 0, TRand(0, 1, tr)),
			TRand(0, 8000, tr),
			TRand(0, 2000, tr)
		);
		LfPulse(TRand(0, 20, tr), 0, TRand(0, 1, tr)) * e
	};
	EqPan2(
		LfPulse(osc() + osc(), 0, 0.5) * 0.04,
		TRand(0, 0.8, tr)
	)
}.OverlapTexture(4, 1, 5).Mix

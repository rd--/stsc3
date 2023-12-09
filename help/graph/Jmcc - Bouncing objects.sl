(* bouncing objects ; jmcc #2 ; lightbulbs, pencils, cans, and other assorted objects ; graph rewrite *)
{ :tr |
	var i = Impulse(TxLine(TRand(3, 7, tr), 600, 4, tr), 0);
	var s = Decay(i * TxLine(0.09, 0.000009, 4, tr), 0.001);
	var r = {
		Ringz(
			s,
			TRand(400, 8400, tr),
			TRand(0.01, 0.1, tr)
		) * TRand(0, 1, tr)
	} !+ 4;
	EqPan2(r, TRand(-1, 1, tr))
}.OverlapTexture(6, 0.01, 4).Mix

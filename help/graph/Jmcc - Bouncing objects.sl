(* bouncing objects ; jmcc #2 ; lightbulbs, pencils, cans, and other assorted objects ; graph rewrite *)
{ :tr |
	var i = Impulse(TrXLine(tr, TrRand(tr, 3, 7), 600, 4), 0);
	var s = Decay(i * TrXLine(tr, 0.09, 0.000009, 4), 0.001);
	var r = {
		Ringz(
			s,
			TrRand(tr, 400, 8400),
			TrRand(tr, 0.01, 0.1)
		) * TrRand(tr, 0, 1)
	} !+ 4;
	EqPan2(r, TrRand(tr, -1, 1))
}.OverlapTexture(6, 0.01, 4)

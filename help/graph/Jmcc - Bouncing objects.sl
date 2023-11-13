(* bouncing objects ; jmcc #2 ; lightbulbs, pencils, cans, and other assorted objects ; graph rewrite *)
{ :tr |
	var i = Impulse(XLine(tr, Rand(tr, 3, 7), 600, 4), 0);
	var s = Decay(i * XLine(tr, 0.09, 0.000009, 4), 0.001);
	var r = {
		Ringz(
			s,
			Rand(tr, 400, 8400),
			Rand(tr, 0.01, 0.1)
		) * Rand(tr, 0, 1)
	} !+ 4;
	EqPan2(r, Rand(tr, -1, 1))
}.OverlapTexture(6, 0.01, 4).Mix

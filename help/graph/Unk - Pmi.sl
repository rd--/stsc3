(* pmi ; texture graph *)
{ :tr |
	var pm = Line(tr, 0, Rand(tr, 0, 12), Rand(tr, 1, 12));
	LinPan2(
		PmOsc(Rand(tr, 0, 2000), Rand(tr, 0, 800), pm, 0),
		Rand(tr, -1, 1),
		0.05
	)
}.OverlapTexture(1, 2, 6).Mix

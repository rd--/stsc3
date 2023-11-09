(* pmi ; texture graph *)
{ :tr |
	var pm = TrLine(tr, 0, TrRand(tr, 0, 12), TrRand(tr, 1, 12));
	LinPan2(
		PmOsc(TrRand(tr, 0, 2000), TrRand(tr, 0, 800), pm, 0),
		TrRand(tr, -1, 1),
		0.05
	)
}.OverlapTexture(1, 2, 7)

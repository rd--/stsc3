(* pmi ; texture graph *)
{ :tr |
	var pm = TLine(0, TRand(0, 12, tr), TRand(1, 12, tr), tr);
	EqPan2(
		PmOsc(TRand(0, 2000, tr), TRand(0, 800, tr), pm, 0),
		TRand(-1, 1, tr)
	) / 20
}.OverlapTexture(1, 2, 6).Mix

(* hell is busy (jmcc) #1 ; graph rewrite *)
{ :tr |
	var e = LfPulse(TrRand(tr, 1, 11), 0, TrRand(tr, 0, 0.7)) * 0.04;
	EqPan2(SinOsc(TrRand(tr, 400, 2400), 0), TrRand(tr, -1, 1)) * e
}.OverlapTexture(4, 4, 8)

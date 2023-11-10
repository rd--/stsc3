(* hell is busy (jmcc) #1 ; graph rewrite *)
{ :tr |
	var e = LfPulse(Rand(tr, 1, 11), 0, Rand(tr, 0, 0.7)) * 0.04;
	EqPan2(SinOsc(Rand(tr, 400, 2400), 0), Rand(tr, -1, 1)) * e
}.OverlapTexture(4, 4, 8)

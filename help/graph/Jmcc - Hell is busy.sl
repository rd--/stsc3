(* hell is busy (jmcc) #1 ; graph rewrite *)
{ :tr |
	var e = LfPulse(TRand(1, 11, tr), 0, TRand(0, 0.7, tr)) * 0.04;
	EqPan2(SinOsc(TRand(400, 2400, tr), 0), TRand(-1, 1, tr)) * e
}.OverlapTexture(4, 4, 8).Mix

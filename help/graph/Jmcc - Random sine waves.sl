(* random sine waves (jmcc) #1 ; graph rewrite *)
{ :tr |
	EqPan2(
		SinOsc(TrRand(tr, 20, 2000), 0),
		TrRand(tr, -1, 1)
	) * 0.05
}.OverlapTexture(5, 2, 9)

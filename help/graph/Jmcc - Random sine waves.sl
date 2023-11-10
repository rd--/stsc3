(* random sine waves (jmcc) #1 ; graph rewrite *)
{ :tr |
	EqPan2(
		SinOsc(Rand(tr, 20, 2000), 0),
		Rand(tr, -1, 1)
	) * 0.05
}.OverlapTexture(5, 2, 9)

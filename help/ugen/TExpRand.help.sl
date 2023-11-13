(* ExpRand ; texture ; requires 0.Max(aUgen) *)
{ :tr |
	EqPan2(
		SinOsc(
			ExpRand(tr, 300, 3000),
			0
		) * 0.Max(SinOsc(ExpRand(tr, 1, 15),0) * 0.05),
		Rand(tr, -1, 1)
	)
}.OverlapTexture(4, 4, 4).Mix

(* ExpRand ; texture ; requires 0.Max(aUgen) *)
{ :tr |
	EqPan2(
		SinOsc(
			TExpRand(300, 3000, tr),
			0
		) * 0.Max(SinOsc(TExpRand(1, 15, tr),0) * 0.05),
		TRand(-1, 1, tr)
	)
}.OverlapTexture(4, 4, 4).Mix

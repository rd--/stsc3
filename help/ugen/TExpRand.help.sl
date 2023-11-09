(* TrExpRand ; texture ; requires 0.Max(aUgen) *)
{ :tr |
	EqPan2(
		SinOsc(
			TrExpRand(tr, 300, 3000),
			0
		) * 0.Max(SinOsc(TrExpRand(tr, 1, 15),0) * 0.05),
		TrRand(tr, -1, 1)
	)
}.OverlapTexture(4, 4, 4)

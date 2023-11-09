(* alien froggies (jmcc) #1 *)
{ :tr |
	var rate = 11;
	var r = Fold(
		rate * TrRand(tr, -0.2, [0.1, 0.2]).Exp,
		1,
		30
	);
	Formant(
		r,
		TrExpRand(tr, [200, 300], 3000),
		TrRand(tr, [0, 1], 9) * r + r
	) * 0.05
}.OverlapTexture(0.5, 0.25, 5)

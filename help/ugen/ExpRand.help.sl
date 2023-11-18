(* ---- ExpRand (jmcc) ; process (Eval) *)
{
	EqPan(
		SinOsc(
			ExpRand(300, 3000),
			0
		) * 0.Max(SinOsc(ExpRand(1, 15),0) * 0.05),
		Rand(-1,1)
	)
}.overlap(4, 4, 4)

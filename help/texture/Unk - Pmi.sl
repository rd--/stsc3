(* pmi *)
{
	var pm = Line(0, Rand(0, 12), Rand(1, 12));
	EqPan(
		PmOsc(Rand(0, 2000), Rand(0, 800), pm, 0),
		Rand(-1, 1)
	) * 0.05
}.overlap(1, 2, 7)

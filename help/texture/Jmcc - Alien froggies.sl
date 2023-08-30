(* alien froggies ; jmcc *)
{
	var r = Fold(11 * 0.2.Rand2.Exp, 1, 30);
	Formant(r, ExpRand(200, 3000), 9.Rand * r + r) * 0.05
}.overlap(0.5, 0.25, 5)

(* lots-o-sins (jmcc) #2 *)
{
	var n = 60;
	{
		SinOscBank(
			{ 40 + LinRand(0, 10000, 0) } ! n,
			nil,
			nil
		)
	} ! 2 * (0.1 / n)
}.overlap(4, 4, 2)

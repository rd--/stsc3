(* tremulate (jmcc) #1 *)
{
	var amp = 0.Max(LfNoise2(Rand(30, 90) ! 4) * 0.1);
	EqPan(
		SinOsc(Rand(500, 900) * [1.0 1.2 1.5 1.8], 0) * amp,
		{ 1.Rand2 } ! 4
	).Sum.CombN(0.1, 0.1, 1)
}.xfade(2, 0.5)

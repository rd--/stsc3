(* random panning sines (jmcc) #4 *)
{
	var n = 8;
	{
		EqPan(
			SinOsc(80 + LinRand(0, 2000, 0), 0),
			LfNoise1(0.4 + 0.8.Rand)
		) * LfNoise1(0.4 + 0.8.Rand).MulAdd(0.4, 0.5)
	} !+ n * 0.4 / n
}.xfade(8, 8)

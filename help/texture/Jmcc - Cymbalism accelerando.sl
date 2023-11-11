(* cymbalism accelerando (jmcc) #2 *)
{
	var p = 15; (* number of partials per channel per 'cymbal' *)
	var f1 = 500 + 2000.Rand;
	var f2 = 8000.Rand;
	var tf = XLine(LinRand(0, 4, 0) + 0.5, 35.Rand + 0.5, 12);
	var t = Impulse(tf, 0);
	{
		RingzBank(
			Decay(t, 0.004) * WhiteNoise() * 0.02,
			{ f1 + f2.Rand } ! p,
			nil,
			{ Rand(1, 4) } ! p
		)
	} ! 2
}.xfade(4, 4)

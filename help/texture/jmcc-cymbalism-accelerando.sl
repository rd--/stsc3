;; cymbalism accelerando (jmcc) #2
{
	var p = 15; (* number of partials per channel per 'cymbal' *)
	var f1 = Rand(500, 2500);
	var f2 = Rand(0, 8000);
	var tf = XLn(LinRand(0, 4, 0) + 0.5, Rand(0, 35) + 0.5, 12);
	var t = Impulse(tf, 0);
	{
		RingzBank(
			Decay(t, 0.004) * WhiteNoise() * 0.02,
			{ f1 + Rand(0, f2) } ! p,
			nil,
			{ Rand(1, 4) } ! p
		)
	} ! 2
}.xfade(4, 4)

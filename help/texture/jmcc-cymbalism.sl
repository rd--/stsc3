;; cymbalism (jmcc) #2
{
	var p = 15;
	var f1 = Rand(500, 2500);
	var f2 = Rand(0, 8000);
	var t = Impulse(Rand(0, 3) + 0.5, 0);
	var z = Decay(t, 0.004) * WhiteNoise() * 0.03;
	{
		RingzBank(
			z,
			{ f1 + Rand(0, f2) } ! p,
			nil,
			{1 + Rand(0, 4)} ! p
		)
	} ! 2
}.xfade(4, 4)

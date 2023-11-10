(* sidereal time (jmcc) #9 *)
var z = { :tr |
	var p = 15;
	var f = XLine(tr, ExpRand(tr, 40, 300), ExpRand(tr, 40, 300), 12);
	var t = [
		LfPulse(f, Rand(tr, 0.1, 0.9), 0),
		0.002,
		LfNoise2(Rand(tr, 0, 8)).Max(0)
	].product;
	{
		{
			Ringz(t, ExpRand(tr, 100, 6000), Rand(tr, 2, 6))
		} !+ p
	} ! 2 * 0.1
}.OverlapTexture(4, 4, 6);
CombN(z, 0.6, Rand(0.1, 0.6), 8) + z.reversed

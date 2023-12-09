(* sidereal time (jmcc) #9 *)
var z = { :tr |
	var p = 15;
	var f = TxLine(
		TExpRand(40, 300, tr),
		TExpRand(40, 300, tr),
		12,
		tr
	);
	var t = [
		LfPulse(f, TRand(0.1, 0.9, tr), 0),
		0.002,
		LfNoise2(TRand(0, 8, tr)).Max(0)
	].product;
	{
		{
			Ringz(t, TExpRand(100, 6000, tr), TRand(2, 6, tr))
		} !+ p
	} ! 2 * 0.1
}.OverlapTexture(4, 4, 6).Mix;
CombN(z, 0.6, Rand(0.1, 0.6), 8) + z.reversed

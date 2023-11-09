(* cymbalism accellerando ; jmcc #2 ; graph rewrite *)
var p = 15;
{ :tr |
	var i = Impulse(
		TrXLine(
			tr,
			TrRand(tr, 0, 4) + 0.5,
			TrRand(tr, 0, 35) + 0.5,
			12
		),
		0
	);
	var s = Decay(i, 0.004) * WhiteNoise() * 0.03;
	var f1 = TrRand(tr, 500, 2500);
	var f2 = TrRand(tr, 0, 8000);
	{
		{
			Ringz(s, f1 + TrRand(tr, 0, f2), TrRand(tr, 1, 5))
		} !+ p
	} ! 2
}.OverlapTexture(4, 4, 3) * 0.1

(* https://soundcloud.com/soundaspureform/harmonic-cloud-1 ; jmcc ; roughguess *)
var n = 48;
{ :tr |
	{
		var f = TrExpRand(tr, 64, 4000).RoundTo(64);
		Lpf(Saw({ TrRand(tr, -1, 1) } ! 2 + f), TrRand(tr, 1, 6) * f) * 0.04
	} !+ n
}.OverlapTexture(0.4, 1, 2)

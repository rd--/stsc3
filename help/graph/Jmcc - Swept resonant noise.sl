(* swept resonant noise (jmcc) #2 ; graph rewrite *)
{ :tr |
	var p = 10;
	var n = WhiteNoise() * 0.007;
	var m = MulAdd(
		SinOsc(0.1 + TrRand(tr, 0, 0.2), 0),
		12 + TrRand(tr, 0, 12),
		60 + TrRand(tr, -24, 24)
	);
	var sweep = Resonz(n, m.MidiCps, 0.1);
	{
		RingzBank(
			sweep,
			{ 80 + TrRand(tr, 0, 10000) } ! p,
			nil,
			{ 0.5 + TrRand(tr, 0, 2) } ! p
		)
	} ! 2
}.OverlapTexture(4, 4, 5) * 0.25

(* repeating harmonic klank (jmcc) *)
{ :tr |
	var p = 8;
	var s = Decay(Dust(0.8) * 0.01, 3.4) * LfSaw(TrRand(tr, 0, 40), 0); (* TrLinRand *)
	var f = TrChoose(tr, [400, 500, 600, 700, 800, 900, 1000, 1200, 1400, 1500, 1600]);
	{
		RingzBank(
			s,
			{ f * TrRand(tr, 1, 13) } ! p,
			1,
			{ TrRand(tr, 0.4, 3.4) } ! p
		)
	} ! 2
}.OverlapTexture(8, 2, 4)

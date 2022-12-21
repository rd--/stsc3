;; repeating harmonic klank (jmcc)
OverlapTexture({ :tr |
	var p = 8;
	var s = Decay(Dust(0.8) * 0.01, 3.4) * LfSaw(TRand(0, 40, tr), 0); (* linrand *)
	var f = TChoose(tr, [400, 500, 600, 700, 800, 900, 1000, 1200, 1400, 1500, 1600]);
	{
		RingzBank(
			s,
			{ f * TRand(1, 13, tr) } ! p,
			1,
			{ TRand(0.4, 3.4, tr) } ! p
		)
	} ! 2
}, 8, 2, 4)

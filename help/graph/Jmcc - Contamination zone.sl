(* contamination zone ; jmcc #9 ; graph rewrite *)
var z = { :tr |
	var f = TrExpRand(tr, 800, 8000);
	var s = PinkNoise() * (LfNoise1(TrRand(tr, 0, 3)) * 0.0008 + 0.0022);
	var r = RingzBank(s, { TrRand(tr, 50, 2000) } ! 4, [1], { TrRand(tr, 0.2, 4) } ! 4).Abs;
	EqPan2(
		Rlpf(r, SinOsc(TrRand(tr, 0, 1), 0) * 0.7 * f + f, 0.1),
		LfNoise1(TrRand(tr, 0, 1))
	) * LfPulse(TrRand(tr, 0, 15), 0, TrRand(tr, 0.2, 0.4))
}.OverlapTexture(8, 3, 4);
6.timesRepeat {
	z := AllpassC(z, 0.040, { Rand(0, 0.04) } ! 2, 16)
};
z * 0.2

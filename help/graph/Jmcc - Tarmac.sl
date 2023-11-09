(* tarmac ; jmcc #10 ; graph rewrite ; requires=TrScramble *)
var a = [
	1 0 0;
	1 1 0;
	1 1 0 0;
	1 1 1 0 0 0;
	1 1 1 0 0 0 0 0;
	1 1 1 1 0 0 0 0;
	1 1 1 1 1 0 0 0;
	1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0
];
{ :tr |
	var t = Impulse(8, 0);
	var i = Demand(t, 0, Dseq(inf, TrScramble(tr, TrChoose(tr, a)))) * t;
	var d = TrRand(tr, 0.05, 0.5);
	var z = PinkNoise() * (LfNoise1(TrRand(tr, 0, 3)) * 0.0008 + 0.0022);
	var s = (
		{ Ringz(z, TrRand(tr, 50, 4000), TrRand(tr, 0.2, 12)) } !+ 4
	).Max(0) * TrChoose(tr, [-1, 1]);
	var f = Rlpf(
		s,
		Decay2(t, 0.004, d) * TrRand(tr, 0, 5000) + TrRand(tr, 0, 100) + 100,
		0.2
	);
	var e = Decay2(i, 0.004, d);
	EqPan2(f, LfNoise1(TrRand(tr, 0, 1))) * e
}.OverlapTexture(12, 3, 6) * 0.2

(* tarmac ; jmcc #10 ; graph rewrite ; requires=TScramble *)
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
	var i = Demand(t, 0, Dseq(inf, Scramble(tr, Choose(tr, a)))) * t;
	var d = Rand(tr, 0.05, 0.5);
	var z = PinkNoise() * (LfNoise1(Rand(tr, 0, 3)) * 0.0008 + 0.0022);
	var s = (
		{ Ringz(z, Rand(tr, 50, 4000), Rand(tr, 0.2, 12)) } !+ 4
	).Max(0) * Choose(tr, [-1, 1]);
	var f = Rlpf(
		s,
		Decay2(t, 0.004, d) * Rand(tr, 0, 5000) + Rand(tr, 0, 100) + 100,
		0.2
	);
	var e = Decay2(i, 0.004, d);
	EqPan2(f, LfNoise1(Rand(tr, 0, 1))) * e
}.OverlapTexture(12, 3, 6).Mix * 0.2

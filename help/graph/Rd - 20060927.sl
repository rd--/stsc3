(* 200060927 ; rd ; texture *)
{ :tr |
	var e = Decay2(
		Impulse({ TrRand(tr, 10, 13) } ! 2, 0),
		TrRand(tr, 0.001, 0.01),
		TrRand(tr, 0.005, 0.02)
	);
	var f = { TrRand(tr, 4, 7) } ! 2 * SinOsc({ TrRand(tr, 10, 13) } ! 2, 0) * e;
	var r = { TrRand(Impulse(0.7, 0), 2220, 2227) } ! 2;
	SinOsc(r, 0) * f * 0.15
}.OverlapTexture(2, 4, 2)

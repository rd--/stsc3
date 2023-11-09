(* saucer base (jmcc) #6 ; graph rewrite *)
{ :tr |
	var b = TrRand(tr, 0, 1000);
	var c = TrRand(tr, 0, 5000);
	var o1 = SinOsc(TrRand(tr, 0, 20), 0) * b + (1.1 * b);
	var o2 = SinOsc(o1, 0) * c + (1.1 * c);
	var o3 = SinOsc(o2, 0) * 0.1;
	EqPan2(o3, TrRand(tr, -1, 1))
}.OverlapTexture(6, 2, 4) * 0.25

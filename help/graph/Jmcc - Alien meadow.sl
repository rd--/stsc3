(* alien meadow (jmcc) #6 *)
{ :tr |
	var z = TrRand(tr, 0, 5000);
	var f = SinOsc(TrRand(tr, 0, 20), 0) * (0.1 * z) + z;
	var a = SinOsc(TrRand(tr, 0, 20), 0) * 0.05 + 0.05;
	EqPan2(SinOsc(f, 0), TrRand(tr, -1, 1)) * a
}.OverlapTexture(6, 2, 6)

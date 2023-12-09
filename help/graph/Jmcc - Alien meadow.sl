(* alien meadow (jmcc) #6 *)
{ :tr |
	var z = TRand(0, 5000, tr);
	var f = SinOsc(TRand(0, 20, tr), 0) * (0.1 * z) + z;
	var a = SinOsc(TRand(0, 20, tr), 0) * 0.05 + 0.05;
	EqPan2(SinOsc(f, 0), TRand(-1, 1, tr)) * a
}.OverlapTexture(6, 2, 6).Mix

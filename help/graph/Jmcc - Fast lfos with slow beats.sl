(* fast lfos with slow beats *)
{ :tr |
	var a0 = TrRand(tr, 40, 240);
	var a1 = a0 + TrRand(tr, -1, 1);
	var a = [a0, a1];
	var b = TrRand(tr, 0, 2000);
	var c = { TrRand(tr, -1, 1) } ! 2 + a;
	SinOsc(SinOsc(a, 0) * TrRand(tr, 0, 1) * b + b, 0) * (SinOsc(c, 0) * 0.05 + 0.05)
}.OverlapTexture(8, 4, 4) * 0.25

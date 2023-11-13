(* fast lfos with slow beats *)
{ :tr |
	var a0 = Rand(tr, 40, 240);
	var a1 = a0 + Rand(tr, -1, 1);
	var a = [a0, a1];
	var b = Rand(tr, 0, 2000);
	var c = { Rand(tr, -1, 1) } ! 2 + a;
	SinOsc(SinOsc(a, 0) * Rand(tr, 0, 1) * b + b, 0) * (SinOsc(c, 0) * 0.05 + 0.05)
}.OverlapTexture(8, 4, 4).Mix * 0.25

(* LinCongC ; texture *)
{ :tr |
	var freq = SampleRate() / 2;
	var m = IRand(tr, 0, 1000000);
	var a = IRand(tr, 1, 2000);
	var c = IRand(tr, 1, 30000);
	LinCongC(freq, a, c, m, { IRand(tr, 0, m) } ! 2) * 0.1
}.OverlapTexture(1, 2, 4)

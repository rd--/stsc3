(* BMoog ; requires=Voicer *)
Voicer(16) { :e |
	var md = TiRand(0, 2, e.w);
	var f0 = LinExp(e.x, 0, 1, 12, 12000);
	var sig = LfSaw([f0 * 0.99, f0 * 1.01], 0) * e.k * 0.4;
	var cf = SinOsc(SinOsc(0.1, 0), 1.5 * pi).kr * e.j * 1550 + 1800;
	var flt = BMoog(sig, cf, e.y, md, 0.95);
	Pan2(
		CombN(flt, 0.5, [0.4, 0.35], 2) * 0.4 + (flt * 0.5),
		e.i * 2 - 1,
		LagUd(e.w, 0, 3) * e.z
	).Sum
}.Mix

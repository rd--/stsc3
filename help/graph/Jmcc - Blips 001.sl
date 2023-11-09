(* blips 001 (jmcc) #SC3d1.5 ; graph rewrite *)
var z = { :tr |
	var blips = {
		var f = TrXLine(
			tr,
			TrExpRand(tr, 0.25, 400),
			TrExpRand(tr, 0.25, 400),
			4
		);
		var nh = TrLine(
			tr,
			TrExpRand(tr, 2, 100),
			TrExpRand(tr, 2, 100),
			4
		);
		Blip(f, nh)
	};
	EqPan2(
		blips() * blips(),
		TrLine(tr, TrRand(tr, -1, 1), TrRand(tr, -1, 1), 4)
	) * 0.3
}.OverlapTexture(2, 1, 12).Distort;
6.timesRepeat {
	z := AllpassN(z, 0.05, { Rand(0, 0.05) } ! 2, 4)
};
z

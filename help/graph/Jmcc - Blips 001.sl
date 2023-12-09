(* blips 001 (jmcc) #SC3d1.5 ; graph rewrite *)
var z = { :tr |
	var blips = {
		var f = TxLine(
			TExpRand(0.25, 400, tr),
			TExpRand(0.25, 400, tr),
			4,
			tr
		);
		var nh = TLine(
			TExpRand(2, 100, tr),
			TExpRand(2, 100, tr),
			4,
			tr
		);
		Blip(f, nh)
	};
	EqPan2(
		blips() * blips(),
		TLine(
			TRand(-1, 1, tr),
			TRand(-1, 1, tr),
			4,
			tr
		)
	) * 0.3
}.OverlapTexture(2, 1, 12).Mix.Distort;
6.timesRepeat {
	z := AllpassN(z, 0.05, { Rand(0, 0.05) } ! 2, 4)
};
z

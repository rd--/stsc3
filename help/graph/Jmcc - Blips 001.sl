(* blips 001 (jmcc) #SC3d1.5 ; graph rewrite *)
var z = { :tr |
	var blips = {
		var f = XLine(
			tr,
			ExpRand(tr, 0.25, 400),
			ExpRand(tr, 0.25, 400),
			4
		);
		var nh = Line(
			tr,
			ExpRand(tr, 2, 100),
			ExpRand(tr, 2, 100),
			4
		);
		Blip(f, nh)
	};
	EqPan2(
		blips() * blips(),
		Line(tr, Rand(tr, -1, 1), Rand(tr, -1, 1), 4)
	) * 0.3
}.OverlapTexture(2, 1, 12).Mix.Distort;
6.timesRepeat {
	z := AllpassN(z, 0.05, { Rand(0, 0.05) } ! 2, 4)
};
z

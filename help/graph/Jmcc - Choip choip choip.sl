(* jmcc ; choip choip choip *)
var dur = 12;
var z = { :tr |
	var i = Impulse(
		XLine(
			tr,
			ExpRand(tr, 1, 30),
			ExpRand(tr, 1, 30),
			dur
		),
		0
	);
	var f = XLine(
		tr,
		ExpRand(tr, 600, 8000),
		ExpRand(tr, 600, 8000),
		dur
	);
	var o = SinOsc(Decay2(i, 0.05, 0.5) * -0.9 * f + f, 0);
	var l = XLine(
		tr,
		ExpRand(tr, 0.01, 0.5),
		ExpRand(tr, 0.01, 0.5),
		dur
	);
	var s = Decay2(i * l, 0.01, 0.2) * o;
	EqPan2(s, Line(tr, Rand(tr, -1, 1), Rand(tr, -1, 1), dur))
}.OverlapTexture(dur - 2, 1, 8);
4.timesRepeat {
	z := AllpassN(z, 0.1, { Rand(0, 0.05) } ! 2, 4)
};
z

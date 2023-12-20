(* tank (jmcc) *)
var r = { :i |
	var l1 = OnePole(LocalIn(2, 0) * 0.98, 0.33);
	var l2 = Rotate2(l1.first, l1.second, 0.23);
	var l3 = AllpassN(l2, 0.05, { Rand(0.01,0.05) } ! 2, 2);
	var l4 = DelayN(l3, 0.3, [0.17, 0.23]);
	var l5 = AllpassN(l4, 0.05, { Rand(0.03,0.15) } ! 2, 2);
	var l6 = LeakDc(l5, 0.995) + i;
	l6 <! LocalOut(l6)
};
var z = {
	EqPan2(
		Decay2(Dust(0.2), 0.1, 0.5) * 0.1 * SinOsc(ExpRand(300, 2200), 0).Cubed,
		Rand(-1, 1)
	)
} !+ 12 + EqPan2(
	Decay2(Dust(0.01), 0.04, 0.3) * BrownNoise(),
	0
);
4.timesRepeat {
	z := AllpassN(z, 0.03, { Rand(0.005, 0.02) } ! 2, 1)
};
r(z)

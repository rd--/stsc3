(* choip (jmcc) #10 *)
{
	var t = 12;
	var i = Impulse(XLine(ExpRand(1, 30), ExpRand(1, 30), t), 0);
	var f = XLine(ExpRand(600, 8000), ExpRand(600, 8000), t);
	var a = SinOsc(Decay2(i, 0.05, 0.5) * (-0.9 * f) + f, 0);
	var l = Line(1.Rand2, 1.Rand2, t);
	var j = XLine(ExpRand(0.01, 0.5), ExpRand(0.01, 0.5), t);
	var z = EqPan(Decay2(i * j, 0.01, 0.2) * a, l);
	4.timesRepeat {
		z := AllpassN(z, 0.1, { 0.05.Rand } ! 2, 4)
	};
	z
}.overlap(10, 1, 8)

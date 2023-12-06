(* http://sccode.org/1-V (nv) L1 *)
var a = { PinkNoise() } ! 2;
50.timesRepeat {
	var f = LinExp(LfNoise1(Rand(0, 0.05)), -1, 1, 40, 15000);
	a := BBandStop(a, f, ExpRand(0.1, 2))
};
Lpf(a, 100000)

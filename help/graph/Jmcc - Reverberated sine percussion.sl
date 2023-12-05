(* reverberated sine percussion (jmcc) #3 *)
var d = 5;
var s = { Resonz(Dust(2 / d) * 50, 200 + Rand(0, 3000), 0.003) } !+ d;
var z = DelayN(s, 0.048, 0.048);
var y = CombL(z, 0.1, LfNoise1({ Rand(0, 0.1) } ! 5) * 0.04 + 0.05, 15).Sum;
var x = y;
4.timesRepeat {
	x := AllpassN(x, 0.050, { Rand(0, 0.05) } ! 2, 1)
};
s + (0.2 * x)

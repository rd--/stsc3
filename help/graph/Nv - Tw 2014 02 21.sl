(* https://twitter.com/headcube/status/437094206767513600 2014-02-21 ; sometimes eventually unstable here (2021-09-24) *)
var x = Impulse(0.05, 0);
{
	20.timesRepeat {
		var a = AllpassL(LeakDc(x, 0.995), 4, 8 ^ LfNoise2(0.1) / 2, 8) * 1.2;
		x := Lpf(a, 8 ^ LfNoise2({ 0.1.Rand } ! 2) * 2500).Tanh
	};
	x * 0.5
} !+ 4

;; https://twitter.com/headcube/status/437094206767513600 ; sometimes eventually unstable here (2021-09-24)
var f = {
	var x = Impulse(0.05, 0);
	var p = {
		var a = AllpassL(LeakDc(x, 0.995), 4, 8 ** LfNoise2(0.1) / 2, 8) * 1.2;
		x := Lpf(a, 8 ** LfNoise2({ 0.1.Rand } ! 2) * 2500).Tanh
	};
	20.timesRepeat(p);
	x * 0.5
};
f !+ 4

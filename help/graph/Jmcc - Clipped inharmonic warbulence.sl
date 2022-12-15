;; clipped inharmonic warbulence ; jmcc
var z = OverlapTexture({ :tr |
	var r = LinExp(LfNoise1(1 / 16), -1, 1, 0.1, 20);
	var f = TRand(24, 96, tr).MidiCps;
	var a = (500 / f).Min(1);
	var n = 12;
	a * (1 .. n).collect({ :i |
		var g = TRand(0, n, tr) + 1;
		var o1 = SinOsc(f * g, 0);
		var o2 = SinOsc(r * TRand(0.9, 1.1, tr), TRand(0, 2 * pi, tr)) * 0.08 - 0.04;
		var o = (o1 * o2.Max(0)).Max(0);
		EqPan2(o, TRand(-1, 1, tr)) * 2 / g
	}).sum;
}, 12.8, 6.4, 6).LeakDc(0.995);
{ CombN(z, 0.3, { Rand(0.1, 0.3) } ! 2, 20) } !+ 8 * 0.2

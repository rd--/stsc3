(* clipped inharmonic warbulence ; jmcc *)
var z = { :tr |
	var r = LinExp(LfNoise1(1 / 16), -1, 1, 0.1, 20);
	var f = TRand(24, 96, tr).MidiCps;
	var a = (500 / f).Min(1);
	var n = 12;
	a * (1 .. n).collect { :i |
		var g = TRand(0, n, tr) + 1;
		var o1 = SinOsc(f * g, 0);
		var o2 = SinOsc(
			r * TRand(0.9, 1.1, tr),
			TRand(0, 2 * pi, tr)
		) * 0.08 - 0.04;
		var o = (o1 * o2.Max(0)).Max(0);
		EqPan2(o, TRand(-1, 1, tr)) * 2 / g
	}.Sum
}.OverlapTexture(12.8, 6.4, 6).Mix;
{
	CombN(LeakDc(z, 0.995), 0.3, { Rand(0.1, 0.3) } ! 2, 20)
} !+ 8 * 0.2

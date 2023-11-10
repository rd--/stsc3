(* clipped inharmonic warbulence ; jmcc *)
var z = { :tr |
	var r = LinExp(LfNoise1(1 / 16), -1, 1, 0.1, 20);
	var f = Rand(tr, 24, 96).MidiCps;
	var a = (500 / f).Min(1);
	var n = 12;
	a * (1 .. n).collect { :i |
		var g = Rand(tr, 0, n) + 1;
		var o1 = SinOsc(f * g, 0);
		var o2 = SinOsc(
			r * Rand(tr, 0.9, 1.1),
			Rand(tr, 0, 2 * pi)
		) * 0.08 - 0.04;
		var o = (o1 * o2.Max(0)).Max(0);
		EqPan2(o, Rand(tr, -1, 1)) * 2 / g
	}.sum
}.OverlapTexture(12.8, 6.4, 6);
{
	CombN(LeakDc(z, 0.995), 0.3, { Rand(0.1, 0.3) } ! 2, 20)
} !+ 8 * 0.2

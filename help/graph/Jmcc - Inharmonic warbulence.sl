(* jmcc ; inharmonic warbulence ; graph rewrite *)
var z = { :tr |
	var f = Rand(tr, 24, 96).MidiCps;
	var a = (500 / f).Min(1);
	var r = XLine(tr, ExpRand(tr, 0.1, 20), ExpRand(tr, 0.1, 20), 25.6);
	var n = 12;
	(1 .. n).collect { :i |
		var g = Rand(tr, 0, n) + 1;
		var m = SinOsc(r * Rand(tr, 0.9, 1.1), Rand(tr, 0, 2 * pi)) * 0.08 - 0.04;
		EqPan2(SinOsc(f * g, 0), Rand(tr, -1, 1)) * m.Max(0) * 2 / g
	}.sum * 0.25
}.OverlapTexture(12.8, 6.4, 6).Mix;
{
	CombN(z, 0.3, { Rand(0.1, 0.3) } ! 2, 8)
} !+ 5 / 2

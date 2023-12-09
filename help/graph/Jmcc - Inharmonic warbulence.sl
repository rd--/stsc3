(* jmcc ; inharmonic warbulence ; graph rewrite *)
var z = { :tr |
	var f = TRand(24, 96, tr).MidiCps;
	var a = (500 / f).Min(1);
	var r = TxLine(TExpRand(0.1, 20, tr), TExpRand(0.1, 20, tr), 25.6, tr);
	var n = 12;
	(1 .. n).collect { :i |
		var g = TRand(0, n, tr) + 1;
		var m = SinOsc(r * TRand(0.9, 1.1, tr), TRand(0, 2 * pi, tr)) * 0.08 - 0.04;
		EqPan2(SinOsc(f * g, 0), TRand(-1, 1, tr)) * m.Max(0) * 2 / g
	}.Sum * 0.25
}.OverlapTexture(12.8, 6.4, 6).Mix;
{
	CombN(z, 0.3, { Rand(0.1, 0.3) } ! 2, 8)
} !+ 5 / 2

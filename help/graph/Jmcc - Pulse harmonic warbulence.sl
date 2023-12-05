(* pulse harmonic warbulence (jmcc) #12 *)
var z = { :tr |
	var f = Rand(tr, 24, 96).MidiCps;
	var r = XLine(tr, ExpRand(tr, 0.1, 20), ExpRand(tr, 0.1, 20), 25.6);
	var n = 12;
	var p = LfPulse(ExpRand(tr, 0.2, 1.2), Rand(tr, 0.1, 0.2), 0.5);
	(1 .. n).collect { :i |
		var m = SinOsc(r * Rand(tr, 0.9, 1.1), Rand(tr, 0, 2 * pi)) * 0.1 - 0.05;
		EqPan2(SinOsc(f * i + f, 0) * m.Max(0), Rand(tr, -1, 1)) / (i + 1)
	}.Sum * p
}.OverlapTexture(12.8, 6.4, 6).Mix;
{
	CombN(z, 0.3, { Rand(0.1, 0.3) } ! 2, 8)
} !+ 5 * 0.25

;; pulse harmonic warbulence (jmcc) #12
var z = OverlapTexture({ :tr |
	var f = TRand(24, 96, tr).MidiCps;
	var r = TxLine(TExpRand(0.1, 20, tr), TExpRand(0.1, 20, tr), 25.6, tr);
	var n = 12;
	var p = LfPulse(TExpRand(0.2, 1.2, tr), TRand(0.1, 0.2, tr), 0.5);
	(1 .. n).collect({ :i |
		var m = (SinOsc(r * TRand(0.9, 1.1, tr), TRand(0, 2 * pi, tr)) * 0.1 - 0.05).max(0);
		Pan2(SinOsc(f * i + f, 0) * m, TRand(-1, 1, tr), 1 / (i + 1))
	}).sum * p
}, 12.8, 6.4, 6);
{ CombN(z, 0.3, { Rand(0.1, 0.3) } ! 2, 8) }.dup(5).sum * 0.25

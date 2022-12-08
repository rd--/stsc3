;; pulse harmonic warbulence (jmcc) #12
{
	var f = Rand(24, 96).MidiCps;
	var r = XLn(ExpRand(0.1, 20), ExpRand(0.1, 20), 25.6);
	var p = LfPulse(ExpRand(0.2, 1.2), Rand(0.1, 0.2), 0.5);
	var z = (1 .. 12).collect({ :i |
		var m = (SinOsc(r * Rand(0.9, 1.1), 2 * pi.Rand) * 0.1 - 0.05).max(0);
		Pan2(SinOsc(f * i, 0) * m * (1 / i), 1.Rand2, 1)
	}).sum * p;
	{ CombN(z, 0.3, { Rand(0.1, 0.3) } ! 2, 8) } !+ 5 * 0.5
}.overlap(12.8, 6.4, 6)

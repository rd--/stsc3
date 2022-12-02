;; inharmonic warbulence (jmcc) #12
{
	var f = Rand(24, 96).MidiCps;
	var a = 1.min(500 / f);
	var r = XLn(ExpRand(0.1, 20), ExpRand(0.1, 20), 25.6);
	var n = 12;
	var z = {
		var g = Rand(1, n + 1);
		var m = (SinOsc(r * Rand(0.9, 1.1), Rand(0, 2 * pi)) * 0.08 - 0.04).max(0);
		Pan2(FSinOsc(f * g, 0) * m * (2 / g), Rand(-1, 1), 1)
	}.dup(n).sum * a;
	{ CombN(z, 0.3, { Rand(0.1, 0.3) } ! 2, 8)	}.dup(5).sum * 0.3
}.overlap(12.8, 6.4, 6)
;; zizle (jmcc) #SC3d1.5
{
	var a = { :f |
		(SinOsc(f * [Rand(0.7, 1.3), 1], { 2 * Rand(0, pi) } ! 2) * 0.1).sum
	};
	var a1 = a(ExpRand(0.3, 0.8)).max(0);
	var a2 = a(ExpRand(6, 24)).abs;
	Pan2(SinOsc(Rand(24, 108).midiCps, 2 * Rand(0, pi)) * a1 * a2, Rand(-1, 1), 1)
}.overlap(4, 4, 12)

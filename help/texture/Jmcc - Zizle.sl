(* zizle (jmcc) #SC3d1.5 *)
{
	var a = { :f |
		(SinOsc(f * [Rand(0.7, 1.3), 1], { 2 * pi.Rand } ! 2) * 0.1).Sum
	};
	var a1 = a(ExpRand(0.3, 0.8)).Max(0);
	var a2 = a(ExpRand(6, 24)).Abs;
	EqPan(
		SinOsc(Rand(24, 108).MidiCps, 2 * pi.Rand) * a1 * a2,
		Rand(-1, 1)
	)
}.overlap(4, 4, 12)

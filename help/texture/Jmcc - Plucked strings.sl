(* plucked strings (jmcc) *)
{
	var i = {
		var s0 = Impulse(Rand(2, 2.2), 0) * 0.3;
		var s1 = Dust(0.5) * 0.3;
		var s2 = Impulse(
			SinOsc(Rand(0.05, 0.15), Rand(0, pi * 2)) * 5 + 5.2,
			0
		) * 0.3;
		[s0, s1, s2].atRandom
	};
	{
		var dt = 1 / Rand(60, 90).Floor.MidiCps;
		var t = Decay(i(), 0.1) * PinkNoise() * 0.1;
		EqPan(CombL(t, dt, dt, 4), 1.Rand2)
	} !+ 5
}.overlap(7, 5, 3)

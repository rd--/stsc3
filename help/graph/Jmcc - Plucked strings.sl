(* Plucked strings (Jmcc) *)
{ :tr |
	var i = {
		var s0 = Impulse(Rand(tr, 2, 2.2), 0) * 0.3;
		var s1 = Dust(0.5) * 0.3;
		var s2 = Impulse(
			SinOsc(Rand(tr, 0.05, 0.15), Rand(tr, 0, pi * 2)) * 5 + 5.2,
			0
		) * 0.3;
		Choose(tr, [s0, s1, s2])
	};
	{
		var dt = 1 / Rand(tr, 60, 90).Floor.MidiCps;
		var t = Decay(i(), 0.1) * PinkNoise() * 0.1;
		EqPan2(CombL(t, dt, dt, 4), Rand(tr, -1, 1))
	} !+ 5
}.OverlapTexture(7, 5, 3)

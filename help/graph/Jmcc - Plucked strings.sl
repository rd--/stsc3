(* Plucked strings (Jmcc) *)
{ :tr |
	var i = {
		var s0 = Impulse(TRand(2, 2.2, tr), 0) * 0.3;
		var s1 = Dust(0.5) * 0.3;
		var s2 = Impulse(
			SinOsc(TRand(0.05, 0.15, tr), TRand(0, pi * 2, tr)) * 5 + 5.2,
			0
		) * 0.3;
		Choose(tr, [s0, s1, s2])
	};
	{
		var dt = 1 / TRand(60, 90, tr).Floor.MidiCps;
		var t = Decay(i(), 0.1) * PinkNoise() * 0.1;
		EqPan2(CombL(t, dt, dt, 4), TRand(-1, 1, tr))
	} !+ 5
}.OverlapTexture(7, 5, 3).Mix

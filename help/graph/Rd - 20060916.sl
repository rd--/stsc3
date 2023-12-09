(* 20060916 ; rd *)
(1 .. 4).collect { :n |
	var tr = Dust(1.6);
	var rd = { :l |
		BufRd(
			1,
			l.asLocalBuf,
			TRand(0, 6, tr),
			0,
			1
		).Lag(LfNoise2(0.3).Range(0.01, 0.1))
	};
	EqPan2(
		SinOsc([60 62 64 65 67 69].rd.MidiCps, 0),
		[-1 -0.5 0 0.25 0.75 1].rd
	) * [0.01 0.05 0.1 0.15 0.25 0.35].rd
}.Mix * 0.25

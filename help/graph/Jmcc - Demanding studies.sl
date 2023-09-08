(* demanding studies (jmcc) *)
|(
	s1 = Drand(inf, [72 75 79 82]),
	s2 = Drand(1, [82 84 86]),
	s3 = Dseq(inf, [72 75 79 s2]),
	x = MouseX(5, 13, 0, 0.2),
	tr = Impulse(x, 0),
	f = Demand(tr, 0, [(s1 - 12).MidiCps, s3.MidiCps]),
	o1 = SinOsc(f + [0 0.7], 0),
	o2 = Saw(f + [0 0.7]) * 0.3,
	o3 = (o1 + o2).Distort
)|
o3 * 0.1

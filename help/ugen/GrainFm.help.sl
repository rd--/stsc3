(* GrainFm ; requires=voicer *)
Voicer(16) { :e |
	var tr = Impulse(e.y * 64 + 10, 0);
	var cf = (e.p * 127 + e.j).MidiCps;
	var mf = (cf * 1.5) + ((1 - e.x) * e.z * cf);
	GrainFm(2, tr, e.k * 0.25, cf, mf, 1 + (e.j * 0.25), e.i, -1, 512) * e.z * LagUd(e.w, 0, 2)
}.Mix

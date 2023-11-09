(* pm-crotale (dmc) #1.7 ; graph rewrite *)
{ :tr |
	var midi = TrIRand(tr, 48, 72);
	var tone = TrRand(tr, 1, 6);
	var art = TrRand(tr, 2, 6) * 3;
	var pan = TrRand(tr, -1, 1);
	var freq = midi.MidiCps;
	var env = Decay2(tr, 0, art);
	var mod = 5 + (1 / TrIRand(tr, 2, 6));
	var amp1 = Decay2(tr, 0, art) * TrRand(tr, 0.1, 0.3);
	var amp2 = Decay2(tr, 0, art * 1.3) * TrRand(tr, 0.1, 0.5);
	var sig = PmOsc(freq, mod * freq, Decay2(tr, 0, art) * tone, 0);
	EqPan2(sig, pan) * amp1 * amp2
}.OverlapTexture(12, 0, 8)

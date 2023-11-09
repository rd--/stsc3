(* noise modulated sines (jmcc) #6 ; graph rewrite *)
var z = { :tr |
	var f = TrRand(tr, 60, 100).MidiCps;
	SinOsc([f, f + 0.2], 0) * LfNoise2(f * [0.15, 0.16]) * 0.05
}.OverlapTexture(4, 4, 4);
CombC(z, 0.3, 0.3, 4) + z.reversed

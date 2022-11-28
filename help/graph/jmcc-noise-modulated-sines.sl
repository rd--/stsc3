;; noise modulated sines (jmcc) #6 ; graph rewrite
var z = OverlapTexture({ :tr |
	var f = TRand(60, 100, tr).MidiCps;
	SinOsc([f, f + 0.2], 0) * LfNoise2(f * [0.15, 0.16]) * 0.05
}, 4, 4, 4);
CombC(z, 0.3, 0.3, 4) + z.reversed

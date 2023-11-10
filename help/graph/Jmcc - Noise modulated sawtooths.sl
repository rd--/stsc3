(* noise modulated sawtooths (jmcc) #6 *)
var a = { :tr |
	var f = Rand(tr, 60, 100).MidiCps;
	LfSaw([f, f + 0.2], 0) * LfNoise2(f * [0.15, 0.16]) * 0.05
}.OverlapTexture(4, 4, 4);
CombC(a, 0.5, 0.5, 4) + a.reversed

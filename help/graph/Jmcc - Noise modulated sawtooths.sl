(* noise modulated sawtooths (jmcc) #6 *)
var a = { :tr |
	var f = TRand(60, 100, tr).MidiCps;
	LfSaw([f, f + 0.2], 0) * LfNoise2(f * [0.15, 0.16]) * 0.05
}.OverlapTexture(4, 4, 4).Mix;
CombC(a, 0.5, 0.5, 4) + a.reversed

(* noise modulated sawtooths (jmcc) #6 *)
{
	var f = Rand(60, 100).MidiCps;
	var z = LfSaw([f, f + 0.2], 0) * LfNoise2(f * [0.15, 0.16]) * 0.1;
	CombN(z, 0.5, 0.5, 4) + z.reversed
}.overlap(4, 4, 4)

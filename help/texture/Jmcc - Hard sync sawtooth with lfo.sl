(* hard sync sawtooth with lfo (jmcc) #6 *)
{
	var f = Rand(30, 80).MidiCps;
	var z = SyncSaw(
		[f, f + 0.2],
		SinOsc(0.2, [0, 2 * pi.Rand]) * 2 * f + (3 * f)
	) * 0.05;
	CombN(z, 0.3, 0.3, 4) + z.reversed
}.overlap(4, 4, 4)

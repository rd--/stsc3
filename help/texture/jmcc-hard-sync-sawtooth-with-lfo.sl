;; hard sync sawtooth with lfo (jmcc) #6
{
	var f = (30 + 50.rand).midiCps;
	var z = SyncSaw([f, f + 0.2], SinOsc(0.2, [0, 2 * pi.rand]) * 2 * f + (3 * f)) * 0.05;
	CombN(z, 0.3, 0.3, 4) + z.reversed
}.overlap(4, 4, 4)
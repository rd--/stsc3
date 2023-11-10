(* hard sync sawtooth with lfo (jmcc) #6 ; graph-rewrite *)
var txt = { :tr |
	var f = (30 + Rand(tr, 0, 50)).MidiCps;
	SyncSaw(
		[f, f + 0.2],
		SinOsc(0.2, { Rand(tr, 0, 2 * pi) } ! 2) * 2 * f + (3 * f)
	) * 0.05
}.OverlapTexture(4, 4, 4);
CombN(txt, 0.3, 0.3, 4) + txt.reversed

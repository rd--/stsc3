(* zizle (jmcc) #SC3d1.5 ; graph rewrite *)
{ :tr |
	var amp = { :f |
		SinOsc(
			f * [TrRand(tr, 0.7, 1.3), 1],
			{ TrRand(tr, 0, 2 * pi) } ! 2
		).sum * 0.1
	};
	var osc = SinOsc(
		TrRand(tr, 24, 108).MidiCps,
		TrRand(tr, 0, 2 * pi)
	);
	EqPan2(
		[
			osc,
			amp(TrExpRand(tr, 0.3, 8)).Max(0),
			amp(TrExpRand(tr, 6, 24)).Abs
		].product,
		TrRand(tr, -1, 1)
	)
}.OverlapTexture(4, 4, 12)

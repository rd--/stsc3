(* zizle (jmcc) #SC3d1.5 ; graph rewrite *)
{ :tr |
	var amp = { :f |
		SinOsc(
			f * [Rand(tr, 0.7, 1.3), 1],
			{ Rand(tr, 0, 2 * pi) } ! 2
		).Sum * 0.1
	};
	var osc = SinOsc(
		Rand(tr, 24, 108).MidiCps,
		Rand(tr, 0, 2 * pi)
	);
	EqPan2(
		[
			osc,
			amp(ExpRand(tr, 0.3, 8)).Max(0),
			amp(ExpRand(tr, 6, 24)).Abs
		].product,
		Rand(tr, -1, 1)
	)
}.OverlapTexture(4, 4, 12).Mix

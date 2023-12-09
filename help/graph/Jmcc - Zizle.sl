(* zizle (jmcc) #SC3d1.5 ; graph rewrite *)
{ :tr |
	var amp = { :f |
		SinOsc(
			f * [TRand(0.7, 1.3, tr), 1],
			{ TRand(0, 2.pi, tr) } ! 2
		).Sum * 0.1
	};
	var osc = SinOsc(
		TRand(24, 108, tr).MidiCps,
		TRand(0, 2.pi, tr)
	);
	EqPan2(
		[
			osc,
			amp(TExpRand(0.3, 8, tr)).Max(0),
			amp(TExpRand(6, 24, tr)).Abs
		].product,
		TRand(-1, 1, tr)
	)
}.OverlapTexture(4, 4, 12).Mix

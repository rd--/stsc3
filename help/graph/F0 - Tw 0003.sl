(* https://sccode.org/1-4Qy ; f0 ; tweet0003 ; texture variant ; requires=LinRand *)
{ :tr |
	var t = SinOsc(
		Rand(tr, 1, 999),
		0
	).Abs;
	{
		Formlet(
			TDmdFor(t, 0, t),
			LinRand(tr, 20, 4000, 0),
			t,
			1 - t
		)
	} ! 2
}.OverlapTexture(15, 1, 7)

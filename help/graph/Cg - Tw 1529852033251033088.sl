;; https://twitter.com/SC2Sbot/status/1529852033251033088 (cg)
Splay(
	Integrator(
		LfNoise0(7 ! 4).RoundTo(SinOsc(4, 0)),
		1
	),
	Saw(
		DmdFor(
			4,
			0,
			Lseq(inf, [1, Lrand(1, [5, 10]), 1, Lrand(1, [11, 20])])
		)
	),
	1,
	0,
	true
).Sin * 0.04

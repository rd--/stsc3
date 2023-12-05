(* https://twitter.com/n_peeq_t/status/1613573427717345282 *)
((1 .. 99) ^ 1.046).collect { :i |
	EqPan(
		SinOsc(99 * i, 0) * i / 99 / 9,
		SinOsc(i / 50, 0)
	) * Perc(
		SinOsc(SinOsc(i / 9999, 3) * 7, 0.5) * 0.5,
		0,
		SinOsc(i / 9999, 0.1) * 0.1,
		-2
	)
}.Sum

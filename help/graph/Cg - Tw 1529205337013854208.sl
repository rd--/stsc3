(* https://twitter.com/SC2Sbot/status/1529205337013854208 (cg) *)
(1 .. 10).collect { :i |
	SinOscFb(
		2000 / i - i,
		Pulse([i / 4, i / 3], 0.5)
	)
}.Mix / 20

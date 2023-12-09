(* https://sccode.org/1-4Qy ; tweet0007 ; texture variant (rd) ; requires=LinRand *)
{ :tr |
	var p = Pluck(
		Crackle([1.9, 1.8]),
		Impulse(TiRand(1, 6, tr), 0),
		0.05,
		TLinRand(0, 0.05, 0, tr),
		1,
		0.5
	);
	Bpf(p, TiRand(0, 1200, tr), 1)
}.OverlapTexture(1, 23, 8).Mix

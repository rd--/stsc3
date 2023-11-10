(* https://sccode.org/1-4Qy ; tweet0007 ; texture variant (rd) ; requires=LinRand *)
{ :tr |
	var p = Pluck(
		Crackle([1.9, 1.8]),
		Impulse(IRand(tr, 1, 6), 0),
		0.05,
		LinRand(tr, 0, 0.05, 0),
		1,
		0.5
	);
	Bpf(p, IRand(tr, 0, 1200), 1)
}.OverlapTexture(1, 23, 7)

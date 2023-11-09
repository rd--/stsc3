(* https://sccode.org/1-4Qy ; tweet0007 ; texture variant (rd) ; requires=TrLinRand *)
{ :tr |
	var p = Pluck(
		Crackle([1.9, 1.8]),
		Impulse(TrIRand(tr, 1, 6), 0),
		0.05,
		TrLinRand(tr, 0, 0.05, 0),
		1,
		0.5
	);
	Bpf(p, TrIRand(tr, 0, 1200), 1)
}.OverlapTexture(1, 23, 7)

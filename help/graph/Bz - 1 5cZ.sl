(* https://sccode.org/1-5cZ ; bz (bernardo zitto) ; graph edit *)
{ :tr |
	var n = 16;
	var sig = {
		var q = TrRand(
			tr,
			TrRand(tr, 0.1, 0.5),
			TrRand(tr, 0.1, 0.5)
		);
		var env = LfNoise1(TrRand(tr, 1, 2)) * TrRand(tr, 1, 2) + TrRand(tr, -2, 0);
		Bpf(
			Dust2(TrRand(tr, 12, 160)) * env,
			TrRand(tr, TrRand(tr, 100, 200), TrRand(tr, 200, 2500)),
			q) / q.Sqrt
	} ! n;
	((sig * TrRand(tr, 1, 2)).Tanh * 0.5).Splay2
}.OverlapTexture(4, 4, 4)

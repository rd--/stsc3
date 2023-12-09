(* https://sccode.org/1-5cZ ; bz (bernardo zitto) ; graph edit *)
{ :tr |
	var n = 16;
	var sig = {
		var q = TRand(
			TRand(0.1, 0.5, tr),
			TRand(0.1, 0.5, tr),
			tr
		);
		var env = LfNoise1(TRand(1, 2, tr)) * TRand(1, 2, tr) + TRand(-2, 0, tr);
		Bpf(
			Dust2(TRand(12, 160, tr)) * env,
			TRand(TRand(100, 200, tr), TRand(200, 2500, tr), tr),
			q) / q.Sqrt
	} ! n;
	((sig * TRand(1, 2, tr)).Tanh * 0.5).Splay2
}.OverlapTexture(4, 4, 4).Mix

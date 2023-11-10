(* https://sccode.org/1-5cZ ; bz (bernardo zitto) ; graph edit *)
{ :tr |
	var n = 16;
	var sig = {
		var q = Rand(
			tr,
			Rand(tr, 0.1, 0.5),
			Rand(tr, 0.1, 0.5)
		);
		var env = LfNoise1(Rand(tr, 1, 2)) * Rand(tr, 1, 2) + Rand(tr, -2, 0);
		Bpf(
			Dust2(Rand(tr, 12, 160)) * env,
			Rand(tr, Rand(tr, 100, 200), Rand(tr, 200, 2500)),
			q) / q.Sqrt
	} ! n;
	((sig * Rand(tr, 1, 2)).Tanh * 0.5).Splay2
}.OverlapTexture(4, 4, 4)

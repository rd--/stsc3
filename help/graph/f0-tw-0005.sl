;; https://sccode.org/1-4Qy ; tweet0005 ; texture variant (rd)
OverlapTexture({ :tr |
	var z = TRand(1, 60, tr);
	var y = LfTri(z, 0).abs / z;
	{ Rlpf(TDmdFor(y, 0, y), z * 99 + y, 0.01) * (6 + y) } ! 2
}, 1, 25, 4)

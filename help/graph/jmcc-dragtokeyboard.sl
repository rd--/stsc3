;; dragtokeyboard (jmcc) ; graph rewrite ; http://www.iamas.ac.jp/~aka/dspss2004/materials/
OverlapTexture({ :tr |
	var in = LfSaw([21000, 21001], 0) * (LfPulse(TExpRand(0.1, 1, tr), 0, 0.3) * 0.2 + 0.02);
	var sr = TExpRand(300, 3000, tr) + [-0.6, 0.6];
	Rlpf(
		in * LfPulse(sr, 0, MouseY(0.01, 0.99, 0, 0.2)),
		sr * (LfPulse(TExpRand(0.1, 12, tr), 0, 0.4) * 0.2 + 0.2 + (LfPulse(TExpRand(0.1, 12, tr), 0, 0.7) * 0.2)),
		0.1
	)
}, 4, 4, 2)

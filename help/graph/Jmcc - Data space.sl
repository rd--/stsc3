;; data space (jmcc) #2 ; graph rewrite
OverlapTexture({ :tr |
	var dt = TRand(0, 0.25, tr) + 0.1;
	var osc = { :n :m |
		var e = LfPulse(TRand(0, m, tr), 0, TRand(0, 1, tr)) * TRand(0, 8000, tr) + TRand(0, 2000, tr);
		LfPulse(TRand(0, n, tr), 0, TRand(0, 1, tr)) * e
	};
	var freq = osc(200, 40) + osc(20, 4) + osc(20, 4);
	CombL(Pan2(LfPulse(freq, 0, 0.5), LfNoise0(TRand(0, 3, tr)) * 0.8, 0.04), dt, dt, 3)
}, 6, 1, 4)

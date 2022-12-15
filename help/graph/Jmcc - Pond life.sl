;; pond life (jmcc) #1 ; graph rewrite ; requires=TLinRand
OverlapTexture({ :tr |
	var f = SinOsc(TRand(20, 50, tr), 0) * TRand(100, 400, tr) + TLinRand(500, 2500, 0, tr);
	var o = SinOsc(f, 0) * LfPulse(3 / TRand(1, 9, tr), 0, TRand(0.2, 0.5, tr)) * 0.04;
	EqPan2(o, TRand(-1, 1, tr))
}, 8, 8, 4)

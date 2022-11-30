;; hell is busy (jmcc) #1 ; graph rewrite
OverlapTexture({ :tr |
	var e = LfPulse(TRand(1, 11, tr), 0, TRand(0, 0.7, tr)) * 0.04;
	Pan2(SinOsc(TRand(400, 2400, tr), 0), TRand(-1, 1, tr), e)
}, 4, 4, 8)

;; hell is busy (jmcc) #1 ; graph rewrite ; left-to-right
OverlapTexture({ :tr |
	var trRand = { :lo :hi | TRand(lo, hi, tr) };
	var e = LfPulse(trRand(1, 11), 0, trRand(0, 0.7)) * 0.04;
	SinOsc(trRand(400, 2400), 0).Pan2(trRand(-1, 1), e)
}, 4, 4, 8)

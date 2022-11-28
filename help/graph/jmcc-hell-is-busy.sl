;; hell is busy (jmcc) #1 ; graph rewrite
OverlapTexture({ :tr |
	var e = LfPulse(TRand(1, 11, tr), 0, TRand(0, 0.7, tr)) * 0.04;
	Pan2(SinOsc(TRand(400, 2400, tr), 0), TRand(-1, 1, tr), e)
}, 4, 4, 8)

;; hell is busy (jmcc) #1 ; graph rewrite ; lefttoright
var trRand = { :tr :lo :hi | TRand(lo, hi, tr) };
OverlapTexture({ :tr |
	var e = LfPulse(tr.trRand(1, 11), 0, tr.trRand(0, 0.7)) * 0.04;
	SinOsc(tr.trRand(400, 2400), 0).Pan2(tr.trRand(-1, 1), e)
}, 4, 4, 8)

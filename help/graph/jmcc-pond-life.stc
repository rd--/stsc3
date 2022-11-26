;; pond life (jmcc) #1 ; graph rewrite ; requires=TLinRand
OverlapTexture({ :tr |
	var f = SinOsc(TRand(20, 50, tr), 0) * TRand(100, 400, tr) + TLinRand(500, 2500, 0, tr);
	var o = SinOsc(f, 0) * LFPulse(3 / TRand(1, 9, tr), 0, TRand(0.2, 0.5, tr)) * 0.04;
	Pan2(o, TRand(-1, 1, tr), 1)
}, 8, 8, 4)

;; pond life (jmcc) #1 ; texture=overlap,8,8,4,inf ; requires=kr
var f = SinOsc(Rand(20, 50), 0) * Rand(100, 400) + LinRand(500, 2500, 0);
var o = SinOsc(f.kr, 0) * LFPulse(3 / Rand(1, 9), 0, Rand(0.2, 0.5)).kr * 0.04;
Pan2(o, Rand(-1, 1), 1)

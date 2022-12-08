;; uplink (jmcc) #2 ; graph rewrite
OverlapTexture({ :tr |
	var osc = {
		var e = LfPulse(TRand(0, 4, tr), 0, TRand(0, 1, tr)) * TRand(0, 8000, tr) + TRand(0, 2000, tr);
		LfPulse(TRand(0, 20, tr), 0, TRand(0, 1, tr)) * e
	};
	Pan2(LfPulse(osc() + osc(), 0, 0.5) * 0.04, TRand(0, 0.8, tr), 1)
}, 4, 1, 5)

;; uplink (jmcc) #2 ; texture=overlap,4,1,5,inf
var osc = {
	var e = LfPulse(Rand(0, 4), 0, Rand(0, 1)) * Rand(0, 8000) + Rand(0, 2000);
	LfPulse(Rand(0, 20), 0, Rand(0, 1)) * e
};
Pan2(LfPulse(osc() + osc(), 0, 0.5) * 0.04, Rand(0, 0.8), 1)

;; pmi ; texture graph
OverlapTexture({ :tr |
	var pm = TLine(0, TRand(0, 12, tr), TRand(1, 12, tr), tr);
	LinPan2(PMOsc(TRand(0, 2000, tr), TRand(0, 800, tr), pm, 0), TRand(-1, 1, tr), 0.05)
}, 1, 2, 7)

;; pmi ; texture=overlap,1,2,7,inf ; requires=kr
var pm = Line(0, Rand(0, 12), Rand(1, 12), 0);
LinPan2(PMOsc(Rand(0, 2000), Rand(0, 800), pm.kr, 0), Rand(-1, 1), 0.05)
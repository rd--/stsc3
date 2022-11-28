;; sawed cymbals (jmcc) ; #10 ; graph rewrite
var p = 15;
OverlapTexture({ :tr |
	var f1 = TRand(500, 2500, tr);
	var f2 = TRand(0, 8000, tr);
	var s = LfSaw(TxLine(TRand(0, 600, tr), TRand(0, 600, tr), 12, tr), 0) * 0.0005;
	{ RingzBank(s, { f1 + TRand(0, f2, tr)} ! p, nil, { TRand(2, 6, tr) } ! p) } ! 2 * 0.1;
}, 4, 4, 6)

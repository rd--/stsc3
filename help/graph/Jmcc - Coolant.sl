;; coolant (jmcc) #2 ; graph rewrite
OverlapTexture({ :tr |
	var p = 10;
	var exc = OnePole(BrownNoise() * 0.002, 0.95);
	{ RingzBank(exc, { 40 + TRand(0, 2000, tr) } ! p, 1, 1) } ! 2
}, 4, 4, 2)

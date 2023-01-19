;; coolant ; jmcc #2 ; graph rewrite
var o = OnePole(BrownNoise() * 0.002, 0.95);
OverlapTexture({ :tr |
	{
		Ringz(o, 40 + TRand(0, 2000, tr), 1) * 0.2
	} !^ 10
}, 6, 6, 3)

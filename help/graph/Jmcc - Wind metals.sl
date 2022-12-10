;; wind metals (jmcc) ; graph rewrite
OverlapTexture({ :tr |
	var n = 6;
	var exc = { BrownNoise() } ! 2 * 0.007 * (LfNoise1(TExpRand(0.125, 0.5, tr)) * 0.75 + 0.25).Max(0);
	var f = { TRand(0, TRand(500, 8000, tr), tr) + TExpRand(60, 4000, tr) } ! n;
	var dt = { TRand(0.1, 2, tr) } ! n;
	var s = RingzBank(exc, f, [1], dt) * 0.1;
	s.SoftClip
}, 5, 2, 12) * 0.1

;; wind metals (jmcc) ; texture=overlap,5,2,12,inf
var n = 6;
var exc = { BrownNoise() } ! 2 * 0.007 * (LfNoise1(ExpRand(0.125, 0.5)) * 0.75 + 0.25).Max(0);
var f = { Rand(0, Rand(500, 8000)) + ExpRand(60, 4000) } ! n;
var dt = { Rand(0.1, 2) } ! n;
var s = RingzBank(exc, f, nil, dt) * 0.1;
s.SoftClip

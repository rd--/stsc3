(* tremulate (jmcc) ; graph rewrite *)
var voiceFunc = { :tr |
	var f = TrRand(tr, 400, 900);
	var r = TrRand(tr, 60, 90);
	var o = SinOsc(f * [1 1.2 1.5 1.8], 0); (* just minor seventh chord *)
	var e = (LfNoise2(r !! 4) * 0.1).Max(0);
	EqPan2(o * e, { Rand(-1, 1) } ! 4).sum
};
CombN(OverlapTexture(voiceFunc, 2, 0.5, 2), 0.1, 0.1, 1)

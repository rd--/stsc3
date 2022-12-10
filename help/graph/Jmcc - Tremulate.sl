;; tremulate (jmcc) ;  graph rewrite
var voiceFunc = { :tr |
	var f = TRand(400, 900, tr);
	var r = TRand(60, 90, tr);
	var o = SinOsc(f * [1.0, 1.2, 1.5, 1.8], 0); ;; just minor seventh chord
	var e = (LfNoise2([r, r, r, r]) * 0.1).Max(0);
	Pan2(o * e, { Rand(-1, 1) } ! 4, 1).sum
};
CombN(OverlapTexture(voiceFunc, 2, 0.5, 2), 0.1, 0.1, 1)

;; tremulate (jmcc) ;  event control ; requires=voicer
var voiceFunc = { :e |
	var s = SinOsc(e.x * 400 + 500 * [1.0, 1.2, 1.5, 1.8], 0); ;; just minor seventh chord
	var a = LfNoise2({ Rand(30, 90) } ! 4 * (0.75 + e.rx)).Max(0) * e.z;
	Pan2(s, { Rand(-1, 1) } ! 4 + (e.o * 2 - 1), a * LagUd(e.w, 0, e.ry * 2)).sum
};
CombN(Voicer(16, voiceFunc).sum * 0.5, 0.1, 0.1, 1)

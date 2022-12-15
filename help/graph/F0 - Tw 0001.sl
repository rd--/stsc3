;; https://sccode.org/1-4Qy ; tweet0001 ; f0 ; graph variant (rd)
var f = { :i |
	var ph = SinOsc(i % 9 // 3 * 100 + (i % 9) + 500, 0);
	var e = LinSeg(1, [0, i * 2, 0, 0.01, 1, 2, 1, 25, 0]);
	EqPan2(SinOsc(i + 1, ph) * 0.03, 1.Rand2) * e
};
(0 .. 98).collect(f).sum

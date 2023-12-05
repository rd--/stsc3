(* https://sccode.org/1-4Qy ; tweet0001 ; f0 ; graph variant (rd) *)
(0 .. 98).collect { :each |
	var ph = SinOsc(each % 9 // 3 * 100 + (each % 9) + 500, 0);
	var e = LinSeg(1, [0, each * 2, 0, 0.01, 1, 2, 1, 25, 0]);
	EqPan(SinOsc(each + 1, ph) * 0.03, 1.Rand2) * e
}.Sum

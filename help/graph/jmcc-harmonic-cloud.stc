;; <https://soundcloud.com/soundaspureform/harmonic-cloud-1> jmcc ; roughguess
var n = 36; (* 48 ; udp *)
OverlapTexture({ :tr |
	var vc = {
		var f = TExpRand(64, 4000, tr).roundTo(64);
		LPF(Saw({ TRand(-1, 1, tr) } ! 2 + f), TRand(1, 6, tr) * f) * 0.04
	};
	Splay2(vc ! n)
}, 0.4, 1, 2)

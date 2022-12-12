;; https://soundcloud.com/soundaspureform/harmonic-cloud-1 ; jmcc ; roughguess
var n = 36; (* 48 ; udp *)
OverlapTexture({ :tr |
	{
		var f = TExpRand(64, 4000, tr).RoundTo(64);
		Lpf(Saw({ TRand(-1, 1, tr) } ! 2 + f), TRand(1, 6, tr) * f) * 0.04
	} !+ n
}, 0.4, 1, 2)

;; <https://swiki.hfbk-hamburg.de/MusicTechnology/899> (tw ; tim walters)
var s = { :k |
	var f = { :i |
		SinOsc(i, SinOsc((i + k) ** i, 0)
		/
		(Decay(Impulse((0.5 ** i) / k, 0), [i, i + 1]) * k))
	};
	(1 .. 6).collect(f).sum
};
(GVerb((0 .. 15).collect(s).sum, 1, 3, 0.5, 0.5, 15, 1, 0.7, 0.5, 300) / 512).transpose.sum
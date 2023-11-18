(* https://swiki.hfbk-hamburg.de/MusicTechnology/899 ; tw (tim walters) *)
(1 .. 8).collect { :k |
	var x = { :f :m :j |
		SinOsc(f + (m * 4 * j), m) * LfNoise1(j + 1 / f * 4) / 2
	};
	var y = x(0.1, 0, 8);
	(1 .. 9).do { :i |
		y := x(i * k * [4, 8], y, i)
	};
	y
}.Mix / 4

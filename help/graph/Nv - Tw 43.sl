(* https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) L43 *)
(0 .. 9).collect { :i |
	var x = Impulse(1, i / 10) + Impulse(0, 0);
	var o = LfSaw([102 101], 0);
	var z = 1.015 ^ Sweep(Impulse(0, 0), 1) * 64 % 1 + 1 * 200;
	var d = 1 / Latch(z, x);
	Pluck(o, x, 1, d, 4, 0.2)
}.Mix / 33

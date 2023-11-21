(* https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) [Line 40] *)
var x = (0 .. 8).collect { :i |
	var t = 0.6 ^ i * 40 * Impulse(2 ^ i / 32, 1 / 2);
	var f = 4 ^ LfNoise0(1 / 16) * 300;
	Rlpf(t, f, 0.005).Sin
}.Splay2;
2.timesRepeat {
	x := FreeVerb2(x.first, x.second, 0.1, 1, 1)
};
x

;; https://sccode.org/1-4Qy ; f0 ; 0345
var f = { :i |
	var c = SinOsc(8 - i / 8, 0);
	SinOsc(1 + i, 0).max(c) * SinOsc(SinOsc(i - 2.1, 0) % SinOsc(9, 1) + (SinOsc(1, 0) > 0 / 3 + 1.83) ** i + 99, c + 1 / 4)
};
Splay2((0.. 7).collect(f)) / 2
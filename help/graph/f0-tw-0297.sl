;; https://sccode.org/1-4Qy ; f0 ; 0297
var f = { :i |
	var c = SinOscFB(i + 1 / [6, 4], 0) / (SinOscFB(1 / [8, 9], 0) + 1.5);
	var d = c % DelayN(c, 0.2, 0.2);
	SinOscFB(1 + c ** i + i * 99, d) * Lag3(d, c % 1 / 99)
};
(0 .. 3).collect(f).Splay2 / 3
